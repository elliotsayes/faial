open Proto
open Sexplib

let call func args =
  Sexp.(
    List ((Atom func)::args)
  )


let nbin_to_string (m:nbin) : string =
  match m with
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "div"
  | Mod -> "mod"

let binop f arg1 arg2 = call f [arg1;arg2]
let unop f arg = call f [arg]

let rec n_ser (a:nexp) : Sexp.t =
  match a with
  | Num n -> Sexp.Atom (string_of_int n)
  | Var x -> Sexp.Atom x
  | Bin (b, a1, a2) ->
    binop (nbin_to_string b) (n_ser a1) (n_ser a2)

let nrel_to_string (r:nrel) =
  match r with
  | NEq -> "="
  | NLe -> "<="
  | NLt -> "<"

let brel_to_string (r:brel) =
  match r with
  | BOr -> "or"
  | BAnd -> "and"

let rec b_ser (b:bexp) : Sexp.t =
  match b with
  | Bool b -> Sexp.Atom (if b then "true" else "false")
  | NRel (b, a1, a2) ->
    binop (nrel_to_string b) (n_ser a1) (n_ser a2)
  | BRel (b, b1, b2) ->
    binop (brel_to_string b) (b_ser b1) (b_ser b2)
  | BNot b -> unop "not" (b_ser b)

let s_ser r =
  call "set" [
    n_ser r.set_elem;
    n_ser r.set_upper_bound;
    b_ser r.set_cond
  ]

let m_ser m = match m with
  | Proto.R -> "ro"
  | Proto.W -> "rw"

let a_ser a =
  let s = a.access_set in
  call (m_ser a.access_mode) [
    n_ser s.set_elem;
    n_ser s.set_upper_bound;
    b_ser s.set_cond
  ]

let t_ser t =
  call "timed" [
    n_ser t.timed_phase;
    a_ser t.timed_data
  ]

let r_ser r =
  call "range" [
    Sexp.Atom r.range_var;
    n_ser r.range_upper_bound;
  ]

let o_ser o =
  call "owned" [
    Sexp.Atom o.owned_tid;
    t_ser o.owned_data
  ]

let stream_ser l =
  Sexp.List (List.map (fun (x,o) -> unop x (o_ser o)) l)

let rec proto_ser p =
  match p with
  | Skip -> Sexp.Atom "skip"
  | Sync -> Sexp.Atom "sync"
  | Acc (x, a) -> binop "loc" (Sexp.Atom x) (a_ser a)
  | Seq (p1, p2) -> binop "begin" (proto_ser p1) (proto_ser p2)
  | Loop (r, p) -> binop "loop" (r_ser r) (proto_ser p)
