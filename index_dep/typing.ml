open Protocols

let typecheck_n (env:Variable.Set.t) (n:Exp.nexp) : bool =
  Variable.Set.subset
    (Freenames.free_names_nexp n Variable.Set.empty)
    env

let typecheck_b (env:Variable.Set.t) (b:Exp.bexp) : bool =
  Variable.Set.subset
    (Freenames.free_names_bexp b Variable.Set.empty)
    env

let typecheck_a (env:Variable.Set.t) (a:Variable.t * Access.t) : bool =
  Variable.Set.subset
    (Freenames.free_names_access (snd a) Variable.Set.empty)
    env

let typecheck_r (env:Variable.Set.t) (r:Range.t) : bool =
  Variable.Set.subset
    (Freenames.free_names_range r Variable.Set.empty)
    env

let rec is_data_exact (env:Variable.Set.t) : Proto.Code.t -> bool =
  function
  | Acc a -> typecheck_a env a
  | Skip | Sync -> true
  | Seq (p, q) -> is_data_exact env p && is_data_exact env q
  | Cond (_, p) -> is_data_exact env p
  | Decl (_, p) -> is_data_exact env p
  | Loop (r, p) ->
    if typecheck_r env r then
      is_data_exact (Variable.Set.add (Range.var r) env) p
    else
      is_data_exact env p

let rec is_control_exact (env:Variable.Set.t) : Proto.Code.t -> bool =
  function
  | Acc _ | Skip | Sync -> true
  | Seq (p, q) -> is_control_exact env p && is_control_exact env q
  | Cond (b, p) -> typecheck_b env b && is_control_exact env p
  | Decl (_, p) -> is_control_exact env p
  | Loop (r, p) ->
    typecheck_r env r &&
    is_control_exact (Variable.Set.add (Range.var r) env) p
