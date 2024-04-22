open Protocols
(*
  1. Generates a summation from a slice.
  2. Flattens a summation expression as a single numeric expression.
  *)

type t =
  | Const of int
  | Sum of Set_range.t * t
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
  | Const x -> string_of_int x
  | Sum (b, s) -> "Σ_" ^ Set_range.to_string b ^ " " ^ to_string s
  | Plus (lhs, rhs) -> "(" ^ to_string lhs ^ " + " ^ to_string rhs ^ ")"

let subst ((x,v): Variable.t * Reals.t) : t -> t =
  let rec subst : t -> t =
    function
    | Const _ as e -> e
    | Plus (lhs, rhs) -> Plus (subst lhs, subst rhs)
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

let rec from_ra : Ra.t -> t =
  function
  | Ra.Tick k -> Const k
  | Ra.Skip -> Const 0
  | Ra.Seq (p, q) -> Plus (from_ra p, from_ra q)
  | Ra.Loop (r, p) -> range_sum r (from_ra p)

let run_ra ~show_code (r: Ra.t) : string =
  let s = from_ra r in
  (if show_code then (to_string s |> print_endline) else ());
  to_string s

