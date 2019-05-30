open Proto
open Sexplib
open Common

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
    Freenames.free_names_steps elems |> StringSet.of_list |> Hashtbl.add result x
  ) h;
  result

let restrict_bexp (b:bexp) (fns:StringSet.t) : bexp =
  let simpl b =
    let new_fn = Freenames.free_names_bexp b [] |> StringSet.of_list in
    if StringSet.is_empty (StringSet.inter fns new_fn) then Bool true
    else b
  in
  let rec iter b =
    match b with
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
  let single_vars = Loops.single_loop_variables p StringSet.empty in
  let single_vars = StringSet.union single_vars (StringSet.of_list k.kernel_variables) in
  let c = Loops.get_constraints p in
  (* 2. Flatten outer loops *)
  let steps = Loops.remove_loops p in
  (* 3. Get the local variables defined in steps *)
  let locals = List.fold_right (fun (_, t) fns -> Freenames.free_names_timed t fns) steps [] in
  let locals = locals |> StringSet.of_list in
  let locals = StringSet.diff locals single_vars in
  (* 3. Make the owner of each access explicit *)
  let steps1, steps2 = Spmd2binary.project_stream locals steps in
  let steps1, steps2 = Constfold.stream_opt steps1, Constfold.stream_opt steps2 in
  (* 4. Perform a constant-fold optimization, to reduce the problem space *)
  let c1, c2 = Spmd2binary.project_condition locals c in
  let pre_and_ x = BRel (BAnd, x, k.kernel_pre) in
  (pre_and_ c1, steps1), (pre_and_ c2, steps2)

type merged = {
  merged_pre: bexp;
  merged_fns: StringSet.t;
  merged_steps: access_t list * access_t list
}

(** Groups two streams together in a single data structure *)

let merge (c1, steps1) (c2, steps2) =
  let pre = b_and c1 c2 in
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
    Hashtbl.add result x {
      merged_fns = fns;
      merged_pre = restrict_bexp pre fns;
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

let serialize_merged m =
  let open Spmd2binary in
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
  let merged_to_bexp m =
    b_and m.merged_pre (steps_to_bexp m.merged_steps (time1, idx1, mode1) (time2, idx2, mode2))
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
          Sexp.List [Sexp.Atom "declare-const"; Sexp.Atom x; Sexp.Atom "Int"]
        )
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
      [
        Sexp.List [Sexp.Atom "assert"; Serialize.b_ser b];
        Sexp.List [Sexp.Atom "check-sat"];
        Sexp.List [Sexp.Atom "get-model"];
        Sexp.List [Sexp.Atom "pop"]
      ]
    ]
  in
  merged_to_bexp m
    |> Constfold.b_opt
    |> serialize_constr m.merged_fns

let join sep elems =
  let on_elem accum x =
    if String.equal accum ""
    then x
    else x ^ sep ^ accum
  in
  List.fold_left on_elem "" elems

let () =
  let print_data data =
    Hashtbl.iter (fun x m ->
      print_string "; Location: ";
      print_endline x;
      serialize_merged m |> List.iter (fun s ->
        Sexp.to_string_hum s |> print_endline;
      );
      (*
      (* Print the locals of each location *)
      print_string "Vars: ";
      join ", " (StringSet.elements m.merged_fns) |> print_endline;
      (* Print the pre-conditions of each location *)
      let b = merged_to_bexp m |> Constfold.b_opt in
      merged_to_bexp m |> Constfold.b_opt |> Serialize.b_ser |> Sexp.to_string_hum |> print_endline;
      *)
      print_endline "";
    ) data;
  in
  let s : Sexp.t = Sexp.input_sexp stdin in
    try
      let d1, d2 = Parse.parse_kernel.run s |> check in
      print_data (merge d1 d2);
    with
    | Parse.ParseError l ->
      List.iter (fun x ->
        print_endline x
      ) l
