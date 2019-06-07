open Proto
open Common

let call func args =
  Sexplib.Sexp.(
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

let rec n_ser (a:nexp) : Sexplib.Sexp.t =
  let open Sexplib in
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

let rec b_ser (b:bexp) : Sexplib.Sexp.t =
  let open Sexplib in
  match b with
  | Bool b -> Sexp.Atom (if b then "true" else "false")
  | NRel (b, a1, a2) ->
    binop (nrel_to_string b) (n_ser a1) (n_ser a2)
  | BRel (b, b1, b2) ->
    binop (brel_to_string b) (b_ser b1) (b_ser b2)
  | BNot b -> unop "not" (b_ser b)
  | Pred (x, v) -> unop x (Sexp.Atom v)

let m_ser m = match m with
  | Proto.R -> "ro"
  | Proto.W -> "rw"

let r_ser r =
  call "range" [
    Sexplib.Sexp.Atom r.range_var;
    n_ser r.range_upper_bound;
  ]

let a_ser a =
  call (m_ser a.access_mode) [n_ser a.access_index; b_ser a.access_cond]

let t_ser t =
  call "timed" [
    n_ser t.timed_phase;
    a_ser t.timed_data
  ]

let serialize_steps name l =
  Sexplib.Sexp.(
    List (Atom name :: (List.map t_ser l))
  )

let serialize_lsteps name l =
  Sexplib.Sexp.(
    List (Atom name :: (List.map (fun (x,o) -> unop x (t_ser o)) l))
  )

let rec proto_ser p =
  let open Sexplib in
  match p with
  | Skip -> Sexp.Atom "skip"
  | Sync -> Sexp.Atom "sync"
  | Assert b -> unop "assert" (b_ser b)
  | Acc (x, a) -> binop "loc" (Sexp.Atom x) (a_ser a)
  | Seq (p1, p2) -> binop "begin" (proto_ser p1) (proto_ser p2)
  | Loop (r, p) -> binop "loop" (r_ser r) (proto_ser p)

let bexp_list_ser name pre =
  let open Sexplib in
  Sexp.(List (Atom name :: (List.map b_ser pre)))

let string_set_ser name s =
  let open Sexplib in
  let l = StringSet.elements s |> List.map (fun x -> Sexp.Atom x) in
  Sexp.List (Sexp.Atom name :: l)

let flat_kernel_ser k =
  let open Loops in
  let open Sexplib in
  Sexp.List [Sexp.Atom "flat";
    bexp_list_ser "pre" k.flat_kernel_pre;
    string_set_ser "single_vars" k.flat_kernel_single_vars;
    string_set_ser "multi_vars" k.flat_kernel_multi_vars;
    serialize_lsteps "steps" k.flat_kernel_steps;
  ]

let location_ser l =
  let open Sexplib in
  let open Sexp in
  let open Spmd2binary in
  List [Atom "location";
    bexp_list_ser "pre" l.location_pre;
    string_set_ser "fns" l.location_fns;
    serialize_steps "steps1" (fst l.location_steps);
    serialize_steps "steps2" (snd l.location_steps);
  ]

let proj_ser (k:Spmd2binary.proj_kernel) : Sexplib.Sexp.t =
  let open Sexplib in
  let open Sexp in
  let open Spmd2binary in
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
    string_set_ser "fns" k.proj_kernel_vars;
    List (Atom "steps" :: elems k.proj_kernel_steps);
  ]
