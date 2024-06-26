open Protocols

(*
  Given an arithmetic expression perform index analysis that yields the
  number of bank conflicts:
    1. remove any offsets that exist, ex `10 + tid` becomes `tid`
    2. evaluate any expression with constants and tids
  *)

(*
  1. If the expressions contains any thread-local variable, return the max
      number of bank conflicts
  2. Remove any uniform offsets that appear in the expression
  3. Try to evaluate the expression, which will only work if the expression
      does _not_ contain any variables.
  4. Otherwise, return the max number of bank conflicts.
*)
let cost
  (params:Config.t)
  (m:Metric.t)
  (thread_locals : Variable.Set.t)
  (n : Exp.nexp)
:
  (int, string) Result.t
=
  let bc_fail (reason : string) : (int, string) Result.t =
    Error (reason ^ ": " ^ Exp.n_to_string n)
  in
  let thread_locals = Variable.Set.diff thread_locals Variable.tid_set in
  let fvs = Exp.n_free_names n Variable.Set.empty in
  let has_thread_locals : bool =
      not (Variable.Set.inter thread_locals fvs |> Variable.Set.is_empty)
  in
  if has_thread_locals then
    bc_fail "Expression uses thread-local variables"
  else
    let ctx = Vectorized.from_config params in
    match Vectorized.to_cost m n ctx with
    | Ok v ->
      Ok v.value
    | Error x ->
      bc_fail x
