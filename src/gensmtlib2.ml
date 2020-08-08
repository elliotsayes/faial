(** Generate First-Order-Logic formula *)

open Exp
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
    let preamble = [
      Serialize.flat_call "set-logic" ["QF_NIA"];
      Serialize.flat_call "set-option" [":produce-models"; "true"];
      Serialize.flat_call "set-option" [":interactive-mode"; "true"];
    ]
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

let print_code : Sexplib.Sexp.t list -> unit =
  List.iter (fun s ->
    Sexplib.Sexp.to_string_hum s |> print_endline;
  )

module Make = functor (Gen: BASE_GEN) ->
struct
  let b_assert b = Serialize.unop "assert" (Gen.b_ser b)

  let l_assert = List.map b_assert

  let ser_predicate p =
    let open Sexplib in
    let open Predicates in
    let g = Predicates.pred_to_codegen p in
    Sexp.List [
      Sexp.Atom "define-fun";
      Sexp.Atom p.pred_name;
      Sexp.List [Serialize.unop g.codegen_arg Gen.uint_s];
      Sexp.Atom "Bool";
      Gen.b_ser g.codegen_body;
    ]

  let define_const v ty =
    let open Sexplib in
    Sexp.List [
      Sexp.Atom "declare-fun";
      Sexp.Atom v.var_name;
      Sexp.List [];
      ty;
    ]

  let define_uint32 var_name =
    [
      define_const var_name Gen.uint_s;
      (* x >= 0 *)
      b_assert (n_ge (Var var_name) (Num 0));
    ]

  let prove l =
    let open Sexplib in
    List.flatten [
      [
        Sexp.List [Sexp.Atom "push"; Sexp.Atom "1";];
      ];
      l;
      [
        Sexp.List [Sexp.Atom "check-sat"];
        Sexp.List [Sexp.Atom "get-model"];
        Sexp.List [Sexp.Atom "pop"; Sexp.Atom "1";];
      ]
    ]

  let serialize_proof p : Sexplib.Sexp.t list =
    let open Symbexp in
    List.(flatten [
      (* Predicates: *)
      map ser_predicate p.proof_preds;
      (* Variable declarations: *)
      map var_make p.proof_decls |> map define_uint32 |> flatten;
      (* Goal of the proof: *)
      [ b_assert p.proof_goal ];
    ]) |> prove

  let serialize_proofs (ps:Symbexp.proof list) : Sexplib.Sexp.t list =
    List.(map serialize_proof ps |> flatten)
end

module Bv2 = Make(BvGen)
module Std2 = Make(StdGen)

let bv_serialize_proofs : Symbexp.proof list -> Sexplib.Sexp.t list = Bv2.serialize_proofs

let int_serialize_proofs : Symbexp.proof list -> Sexplib.Sexp.t list = Std2.serialize_proofs

let translate (ps: Symbexp.proof Stream.t) : Sexplib.Sexp.t list =
  let open Serialize in
  Streamutil.to_list ps
  |> int_serialize_proofs

let print: Sexplib.Sexp.t list -> unit =
  List.iter Serialize.s_print
