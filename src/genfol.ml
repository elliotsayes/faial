(** Generate First-Order-Logic formula *)

open Proto
open Common

module type BASE_GEN = sig
  val preamble : Sexplib.Sexp.t list
  val uint_s : Sexplib.Sexp.t
  val b_ser : bexp -> Sexplib.Sexp.t
end

module StdGen : BASE_GEN =
  struct
    let uint_s = Sexplib.Sexp.Atom "Int"
    let b_ser = Serialize.StdBexp.b_ser
    let preamble = []
  end

module BvGen : BASE_GEN =
  struct
    let uint_s =
      let open Sexplib in
      Sexp.List [
        Sexp.Atom "_";
        Sexp.Atom "BitVec";
        Sexp.Atom "32";
      ]

    let b_ser = Serialize.BvBexp.b_ser

    let preamble = [
      Serialize.flat_call "set-logic" ["QF_BV"];
      Serialize.flat_call "set-option" [":produce-models"; "true"];
      Serialize.flat_call "set-option" [":interactive-mode"; "true"];
    ]
  end

let rec zip l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> []
  | x::l1, y::l2 -> (x,y) :: (zip l1 l2)

module Make = functor (Gen: BASE_GEN) ->
  struct

  let mode_to_nexp m =
    Num (match m with
    | R -> 0
    | W -> 1)

  let access_list_to_bexp elems time idx mode other_mode =
    List.map (fun elem ->
      let result = [
        n_eq time elem.timed_phase;
        n_eq mode (elem.timed_data.access_mode |> mode_to_nexp);
        elem.timed_data.access_cond
      ] @
      List.map (fun (i, j) -> n_eq i j) (zip idx elem.timed_data.access_index);

      in
      (if elem.timed_data.access_mode = R
      then (n_eq other_mode (mode_to_nexp W))::result
      else result) |> b_and_ex
    ) elems |> b_or_ex

  let steps_to_bexp (step1, step2) (time1, idx1, mode1) (time2, idx2, mode2) =
    b_and_ex (
      [
        access_list_to_bexp step1 time1 idx1 mode1 mode2;
        access_list_to_bexp step2 time2 idx2 mode2 mode1;
        n_eq time1 time2;
      ]
      @
      List.map (fun (i,j) -> n_eq i j) (List.combine idx1 idx2)
    )

  let range i j =
    let rec iter n acc =
      if n < i then acc else iter (n-1) (n :: acc)
    in
    iter j []

  let is_even n =
    n mod 2 = 0

  let pow base exponent =
    if exponent < 0 then invalid_arg "exponent can not be negative" else
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
    aux 1 base exponent

  let eq_nums x l : bexp =
    List.map (fun i -> n_eq x (Num i)) l
    |> b_or_ex

  let gen_pow base x : bexp =
    let ub = 0xFFFFFFFF in
    let rec pows n : int list =
      let x = pow base n in
      if x > ub then []
      else if x == ub then [x]
      else x :: pows (n + 1)
    in
    pows 0 |> eq_nums x

  let pred name body =
    let open Sexplib in
    Sexp.List [
      Sexp.Atom "define-fun";
      Sexp.Atom name;
      Sexp.List [Serialize.unop "x" Gen.uint_s];
      Sexp.Atom "Bool";
      body (Var (var_make "x")) |> Gen.b_ser;
    ]

  let define_const v ty =
    let open Sexplib in
    Sexp.List [
      Sexp.Atom "declare-fun";
      Sexp.Atom v.var_name;
      Sexp.List [];
      ty;
    ]

  let b_assert b =
    let open Sexplib in
    Gen.b_ser b
      |> fun b -> Sexp.List [Sexp.Atom "assert"; b]

  let define_uint32 var_name =
    [
      define_const var_name Gen.uint_s;
      (* x >= 0 *)
      b_assert (n_ge (Var var_name) (Num 0));
    ]

  let predicates =
    List.map (fun x ->
      pred ("pow" ^ string_of_int x) (gen_pow x)
    ) (range 2 4)
  |> List.append [
    pred "uint32" (fun x -> n_le x (Num 0xFFFFFFFF));
    pred "uint16" (fun x -> n_le x (Num 0xFFFF));
    pred "uint8" (fun x -> n_le x (Num 0xFF));
  ]

  let generate_kernel k =
    let open Spmd2binary in
    let open Sexplib in
    let mk_var x = Var (var_make x) in
    let time1 = mk_var (tid1 ^ "time$") in
    let time2 = mk_var (tid2 ^ "time$") in
    let mode1 = mk_var (tid1 ^ "mode$") in
    let mode2 = mk_var (tid2 ^ "mode$") in
    let dims = ["x"; "y"; "z"] in
    let idx1 = List.map (fun d -> mk_var (tid1 ^ "idx" ^ d ^ "$")) dims in
    let idx2 = List.map (fun d -> mk_var (tid2 ^ "idx" ^ d ^ "$")) dims in

    let generate_vars =
      let vars = k.proj_kernel_vars in
      let more_vars =
        List.map
          (fun x -> match x with | Var x -> x | _ -> failwith "")
          ([time1; time2; mode1; mode2] @ idx1 @ idx2)
      in
      VarSet.elements vars
        |> List.append more_vars
        |> List.map define_uint32
        |> List.flatten
    in
    let generate_loc b =
      List.flatten [
        [
          Sexp.List [Sexp.Atom "push"];
        ];
        [
          b_assert b;
          Sexp.List [Sexp.Atom "check-sat"];
          Sexp.List [Sexp.Atom "get-model"];
          Sexp.List [Sexp.Atom "pop"]
        ]
      ]
    in
    let gen_steps ss =
      steps_to_bexp ss (time1, idx1, mode1) (time2, idx2, mode2)
        |> generate_loc
    in
    (generate_vars, gen_steps)

  let iter_generated_code k =
    let open Spmd2binary in
    let print_code =
      List.iter (fun s ->
        Sexplib.Sexp.to_string_hum s |> print_endline;
      )
    in
    let decls, gen_steps = generate_kernel k in
    print_endline "; Preamble";
    print_code Gen.preamble;
    print_endline "; Predicates:";
    print_code predicates;
    print_endline "";
    print_endline "; Variables declarations:";
    print_code decls;
    print_endline "";
    print_endline "; Assumptions:";
    print_code (List.map b_assert k.proj_kernel_pre);
    print_endline "";
    Hashtbl.iter (fun x s ->
      print_string "; Location: ";
      print_endline x.var_name;
      print_code (gen_steps s);
      print_endline ""
    ) k.proj_kernel_steps

end

module Bv = Make(BvGen)
module Std = Make(StdGen)


