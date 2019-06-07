(** Generate First-Order-Logic formula *)

open Proto
open Common

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
    steps_to_bexp m.location_steps (time1, idx1, mode1) (time2, idx2, mode2)
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
      ) m.location_pre;
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
    |> serialize_constr m.location_fns

let pred name body =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "define-fun";
    Sexp.Atom name;
    Sexp.List [Serialize.unop "x" (Sexp.Atom "Int")];
    Sexp.Atom "Bool";
    body (Var "x") |> Serialize.b_ser;
  ]
let predicates = [
  pred "pow2" (pow2 31);
  pred "uint32" (fun x -> n_le x (Num 2147483648));
]

let iter_generated_code t =
  print_endline "; Standard-library:";
  List.iter (fun s ->
    Sexplib.Sexp.to_string_hum s |> print_endline;
  ) predicates;
  print_endline "";
  Hashtbl.iter (fun x m ->
    print_string "; Location: ";
    print_endline x;
    serialize_merged m |> List.iter (fun s ->
      Sexplib.Sexp.to_string_hum s |> print_endline;
    );
    print_endline "";
  ) t
