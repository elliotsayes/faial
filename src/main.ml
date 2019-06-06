open Proto
open Common
open Lexing

let group_assoc l =
  let groups = Hashtbl.create 0 in
  let rec iter l =
    match l with
    | (x, a)::l -> begin
      let elems =
        match Hashtbl.find_opt groups x with
        | Some elems -> elems
        | None -> []
      in
      Hashtbl.replace groups x (a::elems);
      iter l
      end
    | [] -> ()
  in
  iter l;
  groups

let extract_free_names h
  : (string, StringSet.t) Hashtbl.t =
  let result = Hashtbl.create 0 in
  Hashtbl.iter (fun x elems ->
    Freenames.free_names_steps elems |> Hashtbl.add result x
  ) h;
  result

let restrict_bexp (fns:StringSet.t) (b:bexp) : bexp =
  let simpl b =
    let new_fn = Freenames.free_names_bexp b StringSet.empty in
    if StringSet.is_empty (StringSet.inter fns new_fn) then Bool true
    else b
  in
  let rec iter b =
    match b with
    | Pred _
    | Bool _ -> b
    | BNot b -> BNot (simpl b)
    | NRel (_, _, _) -> simpl b
    | BRel (o, b1, b2) -> begin
        let b1 = iter b1 |> simpl in
        let b2 = iter b2 |> simpl in
        match b1, b2 with
        | Bool true, b | b, Bool true -> b
        | _, _ -> BRel (o, b1, b2)
      end
  in
  iter b |> Constfold.b_opt

let check (k:kernel) =
  (* 0. Make sure each loop has a synchronization step going on *)
  (* XXX *)
  (* 1. Make sure each loops as a unique variable *)
  let p = Loops.normalize_variables k.kernel_code in
  (* 2. Extract single-valued variables *)
  let single_vars = Loops.single_loop_variables p StringSet.empty in
  let single_vars = StringSet.union single_vars (StringSet.of_list k.kernel_variables) in
  (* 2. Flatten outer loops *)
  let steps = Loops.remove_loops p in
  (* 3. Get the local variables defined in steps *)
  let locals = List.fold_right (fun (_, t) fns -> Freenames.free_names_timed t fns) steps StringSet.empty in
  let locals = StringSet.diff locals single_vars in
  (* 3. Make the owner of each access explicit *)
  let steps1, steps2 = Spmd2binary.project_stream locals steps in
  let steps1, steps2 = Constfold.stream_opt steps1, Constfold.stream_opt steps2 in
  let loop_pre =
    Loops.get_constraints p
    |> List.map Constfold.norm
    |> List.flatten
    |> List.map (Spmd2binary.project_condition locals)
    |> List.flatten
  in
  loop_pre, steps1, steps2

type merged = {
  merged_pre: bexp list;
  merged_fns: StringSet.t;
  merged_steps: access_t list * access_t list
}

(** Groups two streams together in a single data structure *)

let merge pre steps1 steps2 =
  let group1 = group_assoc steps1 in
  let group2 = group_assoc steps2 in
  let fns1 = extract_free_names group1 in
  let fns2 = extract_free_names group2 in
  let result = Hashtbl.create (Hashtbl.length group1) in
  let find_or tb k d =
    match Hashtbl.find_opt tb k with
      | Some v -> v
      | None -> d
  in
  let get_fns x =
    StringSet.union
      (find_or fns1 x StringSet.empty)
      (find_or fns2 x StringSet.empty)
  in
  let add_result x steps1 steps2 =
    let fns = get_fns x in
    let pre = List.map (restrict_bexp fns) pre in
    Hashtbl.add result x {
      merged_fns = List.fold_left (fun new_fns b -> Freenames.free_names_bexp b new_fns) fns pre;
      merged_pre = pre;
      merged_steps = (steps1, steps2)
    }
  in
  Hashtbl.iter (fun x steps1 ->
    let steps2 = find_or group2 x [] in
    Hashtbl.remove group2 x;
    add_result x steps1 steps2
  ) group1;
  Hashtbl.iter (fun x steps2 ->
    add_result x (find_or group1 x []) steps2
  ) group2;
  result

let mode_to_nexp m =
  Num (match m with
  | R -> 0
  | W -> 1)

let access_list_to_bexp elems time idx mode other_mode =
  List.map (fun elem ->
    let result = [
      n_eq time elem.timed_phase;
      n_eq idx elem.timed_data.access_index;
      n_eq mode (elem.timed_data.access_mode |> mode_to_nexp);
      elem.timed_data.access_cond
    ] in
    (if elem.timed_data.access_mode = R
    then (n_eq other_mode (mode_to_nexp W))::result
    else result) |> b_and_ex
  ) elems |> b_or_ex

let steps_to_bexp (step1, step2) (time1, idx1, mode1) (time2, idx2, mode2) =
  b_and_ex [
    access_list_to_bexp step1 time1 idx1 mode1 mode2;
    access_list_to_bexp step2 time2 idx2 mode2 mode1;
    n_eq time1 time2;
    n_eq idx1 idx2;
  ]


let range i j =
  let rec iter n acc =
    if n < i then acc else iter (n-1) (n :: acc)
  in
  iter j []

let pow2 n x : bexp =
    range 0 n
    |> List.map (fun i -> n_eq x (Num (1 lsl i)))
    |> b_or_ex

let serialize_merged m =
  let open Spmd2binary in
  let open Sexplib in
  let time1 = Var (tid1 ^ "time.") in
  let time2 = Var (tid2 ^ "time.") in
  let idx1 = Var (tid1 ^ "idx.") in
  let idx2 = Var (tid2 ^ "idx.") in
  let mode1 = Var (tid1 ^ "mode.") in
  let mode2 = Var (tid2 ^ "mode.") in
  let more_vars =
    List.map
      (fun x -> match x with | Var x -> x | _ -> "")
      [time1; time2; idx1; idx2; mode1; mode2]
  in
  let merged_steps_to_bexp m =
    steps_to_bexp m.merged_steps (time1, idx1, mode1) (time2, idx2, mode2)
  in
  let serialize_constr (vars:StringSet.t) (b:bexp) =
    (*
      (assert (forall ((p Int) (q Int) (n Int) (m Int) (e Exp))
        (=>
          (and
            (can-write p e m)
            (=> (not (= p q)) (not (= n m)))
          )
        (can-write p (write q e n) m)
      )
    *)
    let decl =
      List.append more_vars (StringSet.elements vars)
      |> List.map (fun x ->
          [
            Sexp.List [Sexp.Atom "declare-const"; Sexp.Atom x; Sexp.Atom "Int"];
            (* x >= 0 *)
            Sexp.List [Sexp.Atom "assert"; Serialize.b_ser (n_ge (Var x) (Num 0))];
          ]
        )
      |> List.flatten
    in
    List.flatten [
      [
        Sexp.List [Sexp.Atom "push"];
      ];
      decl;
      if StringSet.mem tid1_s vars || StringSet.mem tid2_s vars
      then [
        Sexp.List [Sexp.Atom "assert";
          Serialize.b_ser (n_neq tid1_t tid2_t)
        ];
      ]
      else [];
      List.map (fun b ->
        Sexp.List [Sexp.Atom "assert";
          Serialize.b_ser b
        ];
      ) m.merged_pre;
      [
        Sexp.List [Sexp.Atom "assert"; Serialize.b_ser b];
        Sexp.List [Sexp.Atom "check-sat"];
        Sexp.List [Sexp.Atom "get-model"];
        Sexp.List [Sexp.Atom "pop"]
      ]
    ]
  in
  merged_steps_to_bexp m
    |> Constfold.b_opt
    |> serialize_constr m.merged_fns

let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let v2_parse input : kernel =
  let filebuf = Lexing.from_channel input in
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position filebuf;
    exit (-1)

let sexp_parse input : kernel =
  let open Sexplib in
  let s : Sexp.t = Sexp.input_sexp input in
    try
      Parse.parse_kernel.run s
    with
    | Parse.ParseError l ->
      List.iter (fun x ->
        print_endline x
      ) l;
      exit (-1)

let pred name body =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "define-fun";
    Sexp.Atom name;
    Sexp.List [Serialize.unop "x" (Sexp.Atom "Int")];
    Sexp.Atom "Bool";
    body (Var "x") |> Serialize.b_ser;
  ]

let main () =
  let open Sexplib in
  pred "exp2" (pow2 31)
  |> Sexp.to_string_hum |> print_endline;
  pred "uint32" (fun x -> n_le x (Num 2147483648))
  |> Sexp.to_string_hum |> print_endline;
  let pre, d1, d2 = v2_parse stdin |> check in
  merge pre d1 d2
  |> Hashtbl.iter (fun x m ->
    print_string "; Location: ";
    print_endline x;
    serialize_merged m |> List.iter (fun s ->
      Sexplib.Sexp.to_string_hum s |> print_endline;
    );
    print_endline "";
  )

let _ = main ()