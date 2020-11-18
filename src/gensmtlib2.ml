(** Generate First-Order-Logic formula *)

open Exp
open Common

module type BASE_GEN = sig
  val preamble : Smtlib.sexp list
  val uint_s : Smtlib.sexp
  val b_ser : bexp -> Smtlib.sexp
end

module StdGen : BASE_GEN =
  struct
    let uint_s = Serialize.symbol "Int"
    let b_ser = Serialize.StdNexp.b_ser
    let preamble = [
      Serialize.flat_call "set-logic" ["QF_NIA"];
      Serialize.flat_call "set-option" [":produce-models"; "true"];
      Serialize.flat_call "set-option" [":interactive-mode"; "true"];
    ]
  end

module BvGen : BASE_GEN =
  struct
    let uint_s =
      let open Smtlib in
      let open Serialize in
      List [
        symbol "_";
        symbol "BitVec";
        symbol "32";
      ]

    let b_ser = Serialize.BvNexp.b_ser

    let preamble = [
      Serialize.flat_call "set-logic" ["QF_BV"];
      Serialize.flat_call "set-option" [":produce-models"; "true"];
      Serialize.flat_call "set-option" [":interactive-mode"; "true"];
    ]
  end

let decl_string name value =
  let open Smtlib in
  let open Predicates in
  let open Serialize in
  [
    List [
      symbol "declare-fun";
      symbol name;
      List [];
      symbol "String";
    ];
    unop "assert" (List [
      symbol "=";
      symbol name;
      Atom (String value)
    ])
  ]

module Make = functor (Gen: BASE_GEN) ->
struct
  let b_assert b = Serialize.unop "assert" (Gen.b_ser b)

  let l_assert = List.map b_assert

  let ser_predicate p =
  let open Smtlib in
  let open Predicates in
  let open Serialize in
    let g = Predicates.pred_to_codegen p in
    List [
      symbol "define-fun";
      symbol p.pred_name;
      List [Serialize.unop g.codegen_arg Gen.uint_s];
      symbol "Bool";
      Gen.b_ser g.codegen_body;
    ]

  let define_const v ty =
  let open Smtlib in
  let open Serialize in
    List [
      symbol "declare-fun";
      symbol v.var_name;
      List [];
      ty;
    ]

  let define_uint32 var_name =
    [
      define_const var_name Gen.uint_s;
      (* x >= 0 *)
      b_assert (n_ge (Var var_name) (Num 0));
    ]

  let prove l =
    let open Serialize in
    let open Smtlib in
    List.flatten [
      [
        List [symbol "push"; Atom (Int 1);];
      ];
      l;
      [
        List [symbol "check-sat"];
        List [symbol "get-model"];
        List [symbol "pop"; Atom (Int 1)];
      ]
    ]

  let serialize_proof p : Smtlib.sexp list =
    let open Symbexp in
    List.(flatten [
      (* String decl *)
      decl_string "$array" p.proof_name;
      (* Predicates: *)
      map ser_predicate p.proof_preds;
      (* Variable declarations: *)
      map var_make p.proof_decls |> map define_uint32 |> flatten;
      (* Goal of the proof: *)
      [ b_assert p.proof_goal ];
    ]) |> prove

  let serialize_proofs (ps:Symbexp.proof list) : Smtlib.sexp list =
    List.(map serialize_proof ps |> flatten)
end

module Bv2 = Make(BvGen)
module Std2 = Make(StdGen)

let bv_serialize_proofs : Symbexp.proof list -> Smtlib.sexp list = Bv2.serialize_proofs

let int_serialize_proofs : Symbexp.proof list -> Smtlib.sexp list = Std2.serialize_proofs

let location_to_sexp (l:Sourceloc.location) : Smtlib.sexp =
    let add_pos (b:Buffer.t) (p:Sourceloc.position) =
        Buffer.add_string b (string_of_int p.pos_line);
        Buffer.add_char b ':';
        Buffer.add_string b (string_of_int p.pos_column)
    in
    let b = Buffer.create 100 in
    Buffer.add_string b l.loc_filename;
    Buffer.add_char b ':';
    add_pos b l.loc_start;
    Buffer.add_char b ':';
    add_pos b l.loc_end;
    let b = Buffer.contents b in
    let open Smtlib in
    let b = atom_to_string (String b) in
    List [Atom (Symbol "echo"); Atom (String b)]

let translate (provenance:bool) ((cache, ps):(Symbexp.LocationCache.t * Symbexp.proof Streamutil.stream)) : Smtlib.sexp Streamutil.stream =
    let open Serialize in
    let open Symbexp in
    let proofs : Smtlib.sexp Streamutil.stream =
        Streamutil.map Std2.serialize_proof ps
        |> Streamutil.map Streamutil.from_list
        |> Streamutil.concat
    in
    if provenance then
        let locs : Smtlib.sexp Streamutil.stream = LocationCache.all cache
            |> Streamutil.from_list
            |> Streamutil.map location_to_sexp
        in
        Streamutil.sequence proofs locs
    else
        proofs

let print: Smtlib.sexp Streamutil.stream -> unit =
  Streamutil.iter (fun x ->
    Serialize.s_print x;
    print_endline "";
  )
