open Stage0
open Protocols
(*
  1. Generates a summation from a slice.
  2. Flattens a summation expression as a single numeric expression.
  *)

type t =
  | Const of int
  | Sum of Set_range.t * t
  | If of Reals.boolean * t * t
  | Plus of t * t

let plus (lhs:t) (rhs:t) : t =
  match lhs, rhs with
  | Const lhs, Const rhs -> Const (lhs + rhs)
  | _, _ -> Plus (lhs, rhs)

let sum (s:Set_range.t) (e:t) =
  match Set_range.count s, e with
  | Some n1, Const n2 -> Const (n1 * n2)
  | _, _ -> Sum (s, e)

let rec to_string : t -> string =
  function
  | If (b, p, q) ->
    "if (" ^ Reals.b_to_string b ^ ") then " ^
    to_string p ^ " else " ^ to_string q
  | Const x -> string_of_int x
  | Sum (b, s) -> "Σ_" ^ Set_range.to_string b ^ " " ^ to_string s
  | Plus (lhs, rhs) -> "(" ^ to_string lhs ^ " + " ^ to_string rhs ^ ")"

let subst ((x,v): Variable.t * Reals.t) : t -> t =
  let rec subst : t -> t =
    function
    | Const _ as e -> e
    | Plus (lhs, rhs) -> Plus (subst lhs, subst rhs)
    | If (b, p, q) ->
      If (Reals.b_subst (x, v) b, subst p, subst q)
    | Sum (b, p)  ->
      let b = Set_range.subst (x, v) b in
      Sum (b,
        if Variable.equal b.var x then
          p
        else
          subst p
      )
  in
  subst

let adapt_error (r:('a, string) Result.t) : ('a, Errors.t) Result.t =
  r
  |> Result.map_error (fun e ->
    let open Errors in
    {output=e; reason=UnsupportedInput}
  )

let range_sum (r:Range.t) (s:t) : t =
  if s = Const 0 then Const 0
  else
    let (o, b) = Set_range.from_range r in
    match o with
    | Some new_range_var ->
      let s = subst (r.var, new_range_var) s in
      sum b s
    | None ->
      sum b s

let rec from_stmt : Ra.Stmt.t -> t =
  function
  | Ra.Stmt.Tick k -> Const k
  | Skip -> Const 0
  | If (b, p, q) -> If (Reals.from_bexp b, from_stmt p, from_stmt q)
  | Seq (p, q) -> Plus (from_stmt p, from_stmt q)
  | Loop (r, p) -> range_sum r (from_stmt p)

let run ~show_code (r: Ra.Stmt.t) : string =
  let s = from_stmt r in
  (if show_code then (to_string s |> print_endline) else ());
  to_string s

