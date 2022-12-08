(** Generate First-Order-Logic formula *)
open Stage0
open Stage1

open Exp

module type BASE_GEN = sig
  val preamble : Smtlib.sexp list
  val uint_s : Smtlib.sexp
  val n_ser : nexp -> Smtlib.sexp
  val b_ser : bexp -> Smtlib.sexp
end

module StdGen : BASE_GEN =
  struct
    let uint_s = Serialize.symbol "Int"
    let b_ser = Serialize.StdNexp.b_ser
    let n_ser = Serialize.StdNexp.n_ser
    let preamble = [
      Serialize.flat_call "set-logic" ["QF_LIA"];
      Serialize.flat_call "set-option" [":produce-models"; "true"];
      Serialize.flat_call "set-option" [":interactive-mode"; "false"];
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
    let n_ser = Serialize.BvNexp.n_ser

    let preamble = [
      Serialize.flat_call "set-logic" ["QF_BV"];
      Serialize.flat_call "set-option" [":produce-models"; "true"];
      Serialize.flat_call "set-option" [":interactive-mode"; "false"];
    ]
  end

let decl_string name value =
  let open Smtlib in
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

  let ser_step_handler (s:Predicates.step_handler) =
    let open Smtlib in
    let open Predicates in
    let open Serialize in
    let g = Predicates.step_to_codegen s in
    List [
      symbol "define-fun";
      symbol s.step_handler_name;
      List [Serialize.unop g.codegen_arg Gen.uint_s];
      Gen.uint_s;
      Gen.n_ser g.codegen_body;
    ]


  let define_const v ty =
  let open Smtlib in
  let open Serialize in
    List [
      symbol "declare-fun";
      symbol (Variable.name v);
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
    let open Proof in
    List.(flatten [
      (* String decl *)
      decl_string "$array" p.array_name;
      (* Predicates: *)
      map ser_predicate p.preds;
      (* Functions: *)
      map ser_step_handler p.funcs;
      (* Variable declarations: *)
      map Variable.from_name p.decls |> map define_uint32 |> flatten;
      (* Goal of the proof: *)
      [ b_assert p.goal ];
    ]) |> prove

  let serialize_proofs (ps:Symbexp.Proof.t list) : Smtlib.sexp list =
    List.(map serialize_proof ps |> flatten)
end

module Bv2 = Make(BvGen)
module Std2 = Make(StdGen)

let bv_serialize_proofs : Symbexp.Proof.t list -> Smtlib.sexp list = Bv2.serialize_proofs

let int_serialize_proofs : Symbexp.Proof.t list -> Smtlib.sexp list = Std2.serialize_proofs

let location_to_sexp (l:Location.t) : Smtlib.sexp =
  let open Location in
  let add_pos (b:Buffer.t) ~line ~column =
    Buffer.add_string b (Index.to_base1 line |> string_of_int);
    Buffer.add_char b ':';
    Buffer.add_string b (Index.to_base1 column |> string_of_int)
  in
  let b = Buffer.create 100 in
  Buffer.add_string b l.filename;
  Buffer.add_char b ':';
  let i = Location.interval l in
  add_pos b ~line:l.line ~column:(Interval.start i);
  Buffer.add_char b ':';
  add_pos b ~line:l.line ~column:(Interval.finish i);
  let b = Buffer.contents b in
  let open Smtlib in
  let b = atom_to_string (String b) in
  List [Atom (Symbol "echo"); Atom (String b)]
(*
let translate (ps:Symbexp.Proof.t Streamutil.stream) : Smtlib.sexp Streamutil.stream =
    let open Serialize in
    let open Symbexp in

    let proofs : Smtlib.sexp Streamutil.stream =
      let open Streamutil in
      let open Smtlib in
      map Std2.serialize_proof ps
      |> map from_list
      |> concat
      |> sequence (from_list [
        List [symbol "push"; Atom (Int 1);];
        List [symbol "assert"; Atom (Symbol "false");];
        List [symbol "check-sat";];
        List [symbol "get-model";];
        List [symbol "pop"; Atom (Int 1);];
      ])
    in
    let locs : unit -> Smtlib.sexp Streamutil.stream = fun _ ->
        LocationCache.all cache
        |> Streamutil.from_list
        |> Streamutil.map location_to_sexp
    in
    Streamutil.lazy_sequence proofs locs

let print: Smtlib.sexp Streamutil.stream -> unit =
  Streamutil.iter (fun x ->
    Serialize.s_print x;
    print_endline "";
  )
*)
