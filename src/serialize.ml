open Exp
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

let s_map (f: 'a -> Sexplib.Sexp.t) (l: 'a list) : Sexplib.Sexp.t =
  List.map f l |> s_list

let s_print (s:Sexplib.Sexp.t) : unit =
  Sexplib.Sexp.to_string_hum s
  |> print_endline

module type NEXP_SERIALIZER = sig
  val n_ser: nexp -> Sexplib.Sexp.t
end

let t_ser t =
  let open Sexplib in
  Sexp.Atom (match t with
    | Task1 -> "$T1"
    | Task2 -> "$T2")

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
    | Proj (t, x) -> binop "proj" (t_ser t) (Sexp.Atom x.var_name)
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
    | Proj (t, x) -> binop "proj" (t_ser t) (Sexp.Atom x.var_name)
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
    let rec nrel_ser (r:nrel) =
      match r with
      | NEq -> binop "="
      | NLe -> binop "<="
      | NLt -> binop "<"
      | NGe -> binop ">="
      | NGt -> binop ">"
      | NNeq -> fun n1 n2 -> unop "not" (nrel_ser NEq n1 n2)

    let rec b_ser (b:bexp) : Sexplib.Sexp.t =
      let open Sexplib in
      match b with
      | Bool b -> Sexp.Atom (if b then "true" else "false")
      | NRel (b, a1, a2) ->
        nrel_ser b (StdNexp.n_ser a1) (StdNexp.n_ser a2)
      | BRel (b, b1, b2) ->
        binop (brel_to_string b) (b_ser b1) (b_ser b2)
      | BNot b -> unop "not" (b_ser b)
      | Pred (x, v) -> unop x (n_ser v)
  end

module BvBexp : BEXP_SERIALIZER =
  struct
    let rec nrel_ser (r:nrel) =
      match r with
      | NEq -> binop "="
      | NLe -> binop "bvule"
      | NLt -> binop "bvult"
      | NGe -> binop "bvuge"
      | NGt -> binop "bvugt"
      | NNeq -> fun n1 n2 -> unop "not" (nrel_ser NEq n1 n2)

    let rec b_ser (b:bexp) : Sexplib.Sexp.t =
      let open Sexplib in
      match b with
      | Bool b -> Sexp.Atom (if b then "true" else "false")
      | NRel (b, a1, a2) ->
        nrel_ser b (BvNexp.n_ser a1) (BvNexp.n_ser a2)
      | BRel (b, b1, b2) ->
        binop (brel_to_string b) (b_ser b1) (b_ser b2)
      | BNot b -> unop "not" (b_ser b)
      | Pred (x, v) -> unop x (n_ser v)
  end

let b_ser = StdBexp.b_ser

let m_ser m = match m with
  | Exp.R -> "ro"
  | Exp.W -> "rw"

let r_ser r =
  call "range" [
    Sexplib.Sexp.Atom r.range_var.var_name;
    n_ser r.range_lower_bound;
    n_ser r.range_upper_bound;
  ]

let a_ser a =
  let idx = Sexplib.Sexp.List (List.map n_ser a.access_index) in
  call (m_ser a.access_mode) [idx]

let rec base_inst_ser (f:'a -> Sexplib.Sexp.t) : 'a base_inst -> Sexplib.Sexp.t =
  let open Sexplib in
  function
  | Base a -> f a
  | Assert b -> unop "assert" (b_ser b)
  | Cond (b, p1) -> call "if" [
      b_ser b;
      base_prog_ser f p1;
    ]
  | Loop (r, p) -> binop "loop" (r_ser r) (base_prog_ser f p)

and base_prog_ser (f:'a -> Sexplib.Sexp.t) (p:('a base_inst) list) : Sexplib.Sexp.t =
  let open Sexplib in
  Sexp.List (List.map (base_inst_ser f) p)

let expr_acc_ser (x, a) : Sexplib.Sexp.t =
  let open Sexplib in
  call "loc" [Sexp.Atom x.var_name; a_ser a]

let acc_sym_ser (x, a, t) : Sexplib.Sexp.t =
  let open Sexplib in
  call "loc" [Sexp.Atom x.var_name; a_ser a; t_ser t]

let a_inst_ser (f: 'a -> Sexplib.Sexp.t) : 'a a_inst -> Sexplib.Sexp.t =
  function
    | Goal b -> unop "goal" (b_ser b)
    | Acc a -> f a

let acc_inst_ser : acc_inst -> Sexplib.Sexp.t =
  a_inst_ser expr_acc_ser

let sync_unsync_ser : sync_unsync -> Sexplib.Sexp.t =
  let open Sexplib in
  function
    | Sync -> Sexp.Atom "sync"
    | Unsync a -> acc_inst_ser a

let inst_ser : inst -> Sexplib.Sexp.t =
  base_inst_ser sync_unsync_ser

let prog_ser : prog -> Sexplib.Sexp.t =
  s_map inst_ser
(*
let u_inst_ser : u_inst -> Sexplib.Sexp.t =
  base_inst_ser acc_inst_ser

let u_prog_ser : u_prog -> Sexplib.Sexp.t =
  s_map u_inst_ser

let s_inst_ser : s_inst -> Sexplib.Sexp.t =
  base_inst_ser (fun x -> call "phase" (List.map u_inst_ser x))

let s_prog_ser : s_prog -> Sexplib.Sexp.t =
  s_map s_inst_ser
*)
let bexp_list pre = List.map b_ser pre

let bexp_list_ser name pre = bexp_list pre |> call name

let var_set_ser name (s:VarSet.t) =
  VarSet.elements s
    |> List.map (fun x -> x.var_name)
    |> atoms
    |> call name

let kernel_ser (f:'a -> Sexplib.Sexp.t) (k:'a kernel) =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    unop "pre" (b_ser k.kernel_pre);
    var_set_ser "locations" k.kernel_locations;
    var_set_ser "locals" k.kernel_local_variables;
    var_set_ser "globals" k.kernel_global_variables;
    f k.kernel_code;
  ]
