open Proto
open Common

let s_list elems =
  Sexplib.Sexp.List elems

let call func args =
  s_list ((Atom func)::args)

let atoms : string list -> Sexplib.Sexp.t list =
  List.map (fun x -> Sexplib.Sexp.Atom x)

let flat_call func args = atoms args |> call func

let binop f arg1 arg2 = call f [arg1;arg2]
let unop f arg = call f [arg]

module type NEXP_SERIALIZER = sig
  val n_ser: nexp -> Sexplib.Sexp.t
end

let t_ser t =
  let open Sexplib in
  Sexp.Atom (match t with
    | Task1 -> "1"
    | Task2 -> "2")

module StdNexp : NEXP_SERIALIZER = struct
  let nbin_to_string (m:nbin) : string =
    match m with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "div"
    | Mod -> "mod"

  let rec n_ser (a:nexp) : Sexplib.Sexp.t =
    let open Sexplib in
    match a with
    | Proj (t, n) -> binop "proj" (t_ser t) (n_ser n)
    | Num n -> Sexp.Atom (string_of_int n)
    | Var x -> Sexp.Atom x.var_name
    | Bin (b, a1, a2) ->
      binop (nbin_to_string b) (n_ser a1) (n_ser a2)
end

module BvNexp : NEXP_SERIALIZER = struct
  let nbin_to_string (m:nbin) : string =
    match m with
    | Plus -> "bvadd"
    | Minus -> "bvusub"
    | Mult -> "bvmul"
    | Div -> "bvudiv"
    | Mod -> "bvurem"

  let rec n_ser (a:nexp) : Sexplib.Sexp.t =
    let open Sexplib in
    match a with
    | Proj (t, n) -> binop "proj" (t_ser t) (n_ser n)
    | Num n -> Sexp.List [
        Sexp.Atom "_";
        Sexp.Atom ("bv" ^ (string_of_int n));
        Sexp.Atom "32";
      ]
    | Var x -> Sexp.Atom x.var_name
    | Bin (b, a1, a2) ->
      binop (nbin_to_string b) (n_ser a1) (n_ser a2)
end

let n_ser = StdNexp.n_ser

let brel_to_string (r:brel) =
  match r with
  | BOr -> "or"
  | BAnd -> "and"

module type BEXP_SERIALIZER = sig
  val b_ser: bexp -> Sexplib.Sexp.t
end

module StdBexp : BEXP_SERIALIZER =
  struct
    let nrel_to_string (r:nrel) =
      match r with
      | NEq -> "="
      | NLe -> "<="
      | NLt -> "<"
      | NGe -> ">="
      | NGt -> ">"

    let rec b_ser (b:bexp) : Sexplib.Sexp.t =
      let open Sexplib in
      match b with
      | Bool b -> Sexp.Atom (if b then "true" else "false")
      | NRel (b, a1, a2) ->
        binop (nrel_to_string b) (StdNexp.n_ser a1) (StdNexp.n_ser a2)
      | BRel (b, b1, b2) ->
        binop (brel_to_string b) (b_ser b1) (b_ser b2)
      | BNot b -> unop "not" (b_ser b)
      | Pred (x, v) -> unop x (Sexp.Atom v.var_name)
  end

module BvBexp : BEXP_SERIALIZER =
  struct
    let nrel_to_string (r:nrel) =
      match r with
      | NEq -> "="
      | NLe -> "bvule"
      | NLt -> "bvult"
      | NGe -> "bvuge"
      | NGt -> "bvugt"

    let rec b_ser (b:bexp) : Sexplib.Sexp.t =
      let open Sexplib in
      match b with
      | Bool b -> Sexp.Atom (if b then "true" else "false")
      | NRel (b, a1, a2) ->
        binop (nrel_to_string b) (BvNexp.n_ser a1) (BvNexp.n_ser a2)
      | BRel (b, b1, b2) ->
        binop (brel_to_string b) (b_ser b1) (b_ser b2)
      | BNot b -> unop "not" (b_ser b)
      | Pred (x, v) -> unop x (Sexp.Atom v.var_name)
  end

let b_ser = StdBexp.b_ser

let m_ser m = match m with
  | Proto.R -> "ro"
  | Proto.W -> "rw"

let r_ser r =
  call "range" [
    Sexplib.Sexp.Atom r.range_var.var_name;
    n_ser r.range_upper_bound;
  ]

let a_ser a =
  let idx = Sexplib.Sexp.List (List.map n_ser a.access_index) in
  call (m_ser a.access_mode) [idx; b_ser a.access_cond]

let t_ser t =
  call "timed" [
    n_ser t.timed_phase;
    a_ser t.timed_data
  ]

let serialize_steps name l =
  List.map t_ser l |> call name

let serialize_lsteps name l =
  List.map (fun (x,o) -> unop x.var_name (t_ser o)) l |> call name

let rec proto_ser p =
  let open Sexplib in
  match p with
  | Skip -> Sexp.Atom "skip"
  | Sync -> Sexp.Atom "sync"
  | Goal b -> unop "goal" (b_ser b)
  | Assert b -> unop "assert" (b_ser b)
  | Acc (x, a) -> binop "loc" (Sexp.Atom x.var_name) (a_ser a)
  | Seq (p1, p2) -> binop "begin" (proto_ser p1) (proto_ser p2)
  | Loop (r, p) -> binop "loop" (r_ser r) (proto_ser p)

let bexp_list pre = List.map b_ser pre

let bexp_list_ser name pre = bexp_list pre |> call name

let var_set_ser name (s:VarSet.t) =
  VarSet.elements s
    |> List.map (fun x -> x.var_name)
    |> atoms
    |> call name

let kernel_ser k =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    var_set_ser "locations" k.kernel_locations;
    var_set_ser "locals" k.kernel_local_variables;
    var_set_ser "globals" k.kernel_global_variables;
    proto_ser k.kernel_code;
  ]

let flat_kernel_ser k =
  let open Loops in
  let open Sexplib in
  Sexp.List [Sexp.Atom "flat-kernel";
    bexp_list_ser "pre" k.flat_kernel_pre;
    call "proofs" (List.map (fun x -> bexp_list x |> s_list) k.flat_kernel_proofs);
    var_set_ser "globals" k.flat_kernel_single_vars;
    var_set_ser "locals" k.flat_kernel_multi_vars;
    serialize_lsteps "steps" k.flat_kernel_steps;
  ]

let proj_ser (k:Taskproj.proj_kernel) : Sexplib.Sexp.t =
  let open Sexplib in
  let open Sexp in
  let open Taskproj in
  let elems t = hashtbl_elements t
    |> List.map (fun (k,v) -> List [
      Atom k;
      serialize_steps "steps1" (fst v);
      serialize_steps "steps2" (snd v);
    ])
  in
  List [
    Atom "proj-kernel";
    bexp_list_ser "pre" k.proj_kernel_pre;
    call "proofs" (List.map (fun x -> bexp_list x |> s_list) k.proj_kernel_proofs);
    var_set_ser "vars" k.proj_kernel_vars;
    List (Atom "steps" :: elems k.proj_kernel_steps);
  ]
