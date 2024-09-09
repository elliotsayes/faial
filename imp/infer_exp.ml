open Protocols

type n =
  | Var of Variable.t
  | Num of int
  | Unary of N_unary.t * t
  | Binary of N_binary.t * t * t
  | NCall of string * t
  | NIf of t * t * t
  | Other of t

and b =
  | Bool of bool
  | NRel of N_rel.t * t * t
  | BRel of B_rel.t * t * t
  | BNot of t
  | Pred of string * t

and t =
  | NExp of n
  | BExp of b
  | Unknown of string

let rec to_string : t -> string =
  function
  | NExp e -> to_n_string e
  | BExp e -> to_b_string e
  | Unknown x -> x

and to_n_string : n -> string =
  function
  | Var x -> Variable.name x
  | Num x -> string_of_int x
  | Unary (o, e) -> N_unary.to_string o ^ " (" ^ to_string e ^ ")"
  | Binary (o, l, r) ->
    "(" ^ to_string l ^ ") " ^ N_binary.to_string o ^ " (" ^ to_string r ^ ")"
  | NCall (o, e) -> o ^ "(" ^ to_string e ^ ")"
  | NIf (e1, e2, e3) ->
    let e1 = to_string e1 in
    let e2 = to_string e2 in
    let e3 = to_string e3 in
    "(" ^ e1 ^ ") ? (" ^ e2 ^ ") : (" ^ e3 ^ ")"
  | Other e -> "$other(" ^ to_string e ^ ")"

and to_b_string : b -> string =
  function
  | Bool b -> if b then "true" else "false"
  | NRel (o, e1, e2) ->
    let o = N_rel.to_string o in
    let e1 = to_string e1 in
    let e2 = to_string e2 in
    "(" ^ e1 ^ ") " ^ o ^ " (" ^ e2 ^ ")"
  | BRel (o, e1, e2) ->
    let o = B_rel.to_string o in
    let e1 = to_string e1 in
    let e2 = to_string e2 in
    "(" ^ e1 ^ ") " ^ o ^ " (" ^ e2 ^ ")"
  | BNot e ->
    "!(" ^ to_string e ^ ")"
  | Pred (o, e) ->
    o ^ "(" ^ to_string e ^ ")"

let or_ (e1:t) (e2:t) : b =
  BRel (BOr, e1, e2)

let n_eq (e1:t) (e2:t) : b =
  NRel (Eq, e1, e2)

let thread_equal (e:t) : b =
  n_eq e (NExp (Other e))

let thread_distinct (e:t) : b =
  NRel (Neq, e, NExp (Other e))

let num (n:int) : t =
  NExp (Num n)

let bool (b:bool) : t =
  BExp (Bool b)

let unknown (lbl:string) : t =
  Unknown lbl

(*
   Parsing expressions requires a global state
   which is a set of unknowns. Whenever we parse a
   statement we may create certain unknowns. Such variables
   are defined within the scope of that statement alone.
   The state UnknownSt is global to parsing numerical
   and boolean expressions.
 *)
module Context = struct
  type t = Variable.Set.t

  let make : t = Variable.Set.empty

  let is_empty : t -> bool =
    Variable.Set.is_empty

  let create (label:string) (st:t) : t * Variable.t =
    let count = Variable.Set.cardinal st in
    let v =
      ("@Unknown" ^ string_of_int count)
      |> Variable.from_name
      |> Variable.set_label label
    in
    Variable.Set.add v st, v

  let get (st:t) : Variable.Set.t =
    st

  let convert (handler:t -> 'a -> t * 'b) (n:'a) : Variable.Set.t * 'b =
    let (u, n) = handler make n in
    (get u, n)

  let rec mmap (f:t -> 'a -> t * 'b) (st:t) : 'a list -> (t * 'b list) =
    function
    | [] -> (st, [])
    | x :: l ->
      let (st, x) = f st x in
      let (st, l) = mmap f st l in
      (st, x :: l)

end

let rec handle_n (u:Context.t) (e:t) : (Context.t * Exp.nexp) =
  match e with
  | NExp n ->
    (match n with
    | Var x -> (u, Exp.Var x)
    | Num x -> (u, Exp.Num x)
    | Binary (o, n1, n2) ->
      let (u, n1) = handle_n u n1 in
      let (u, n2) = handle_n u n2 in
      (u, Exp.Binary (o, n1, n2))
    | Other n ->
      let (u, n) = handle_n u n in
      (u, Exp.Other n)
    | Unary (o, n) ->
      let (u, n) = handle_n u n in
      (u, Exp.Unary (o, n))
    | NCall (x, n) ->
      let (u, n) = handle_n u n in
      (u, Exp.NCall (x, n))
    | NIf (b, n1, n2) ->
      let (u, b) = handle_b u b in
      let (u, n1) = handle_n u n1 in
      let (u, n2) = handle_n u n2 in
      (u, Exp.NIf (b, n1, n2)))
  | BExp _ ->
    let (u, b) = handle_b u e in
    (u, Exp.cast_int b)
  | Unknown lbl ->
    let (u, x) = Context.create lbl u in
    (u, Exp.Var x)

and handle_b (u:Context.t) (e:t) : (Context.t * Exp.bexp) =
  match e with
  | BExp b ->
    (match b with
    | Bool x -> (u, Exp.Bool x)
    | NRel (o, n1, n2) ->
      let (u, n1) = handle_n u n1 in
      let (u, n2) = handle_n u n2 in
      (u, NRel (o, n1, n2))
    | BRel (o, b1, b2) ->
      let (u, b1) = handle_b u b1 in
      let (u, b2) = handle_b u b2 in
      (u, BRel (o, b1, b2))
    | BNot b ->
      let (u, b) = handle_b u b in
      (u, BNot b)
    | Pred (x, n) ->
      let (u, n) = handle_n u n in
      (u, Pred (x, n)))
  | NExp _ ->
    let (u, n) = handle_n u e in
    (u, Exp.cast_bool n)
  | Unknown lbl ->
    let (u, x) = Context.create lbl u in
    (u, Exp.cast_bool (Var x))

(* Convert a d_nexp into an nexp and get the set of unknowns *)
let to_nexp: t -> Variable.Set.t * Exp.nexp =
  Context.convert handle_n

(* Convert a d_bexp into an bexp and get the set of unknowns *)
let to_bexp: t -> Variable.Set.t * Exp.bexp =
  Context.convert handle_b


let to_nexp_list: t list -> Variable.Set.t * Exp.nexp list =
  Context.convert (Context.mmap handle_n)

(* Convert a d_nexp into an nexp only if there are no unknowns *)
let try_to_nexp (n:t) : Exp.nexp option =
  let (u, n) = handle_n Context.make n in
  if Context.is_empty u
  then Some n
  else None

(* Convert a d_bexp into an bexp only if there are no unknowns *)
let try_to_bexp (b:t) : Exp.bexp option =
  let (u, b) = handle_b Context.make b in
  if Context.is_empty u
  then Some b
  else None

let as_decls (xs:Variable.Set.t) : Decl.t list =
  Variable.Set.elements xs |> List.map Decl.unset

let decl_unknown (vars:Variable.Set.t) : Stmt.t list =
  if Variable.Set.is_empty vars then []
  else
    [Decl (as_decls vars)]

let infer_nexp (e:t) : (Stmt.t list * Exp.nexp) =
  let (decls, e) = to_nexp e in
  let decls = decl_unknown decls in
  (decls, e)
