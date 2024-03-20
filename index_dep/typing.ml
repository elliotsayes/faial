open Protocols

let typecheck_n (env:Variable.Set.t) (n:Exp.nexp) : bool =
  Variable.Set.subset
    (Freenames.free_names_nexp n Variable.Set.empty)
    env

let typecheck_b (env:Variable.Set.t) (b:Exp.bexp) : bool =
  Variable.Set.subset
    (Freenames.free_names_bexp b Variable.Set.empty)
    env

let typecheck_a (env:Variable.Set.t) (a:Access.t) : bool =
  Variable.Set.subset
    (Freenames.free_names_access a Variable.Set.empty)
    env

let typecheck_r (env:Variable.Set.t) (r:Range.t) : bool =
  Variable.Set.subset
    (Freenames.free_names_range r Variable.Set.empty)
    env

module Context = struct
  type t =
    | Cond of Exp.bexp * t
    | Decl of {var: Variable.t; ty: C_type.t; body: t}
    | Loop of Range.t * t
    | Acc of Variable.t * Access.t

  let rec from_proto : Proto.Code.t -> t Seq.t =
    function
    | Acc (x, y) -> Seq.return (Acc (x, y))
    | Sync _ | Skip -> Seq.empty
    | Cond (b, p) ->
      from_proto p |> Seq.map (fun c -> Cond (b, c))
    | Loop (r, p) ->
      from_proto p |> Seq.map (fun c -> Loop (r, c))
    | Seq (p, q) ->
      from_proto p |> Seq.append (from_proto q)
    | Decl {var; ty; body} ->
      from_proto body |> Seq.map (fun body -> Decl {var; ty; body})

  let rec is_control_independent (env:Variable.Set.t) : t -> bool =
    function
    | Cond (b, p) ->
      typecheck_b env b &&
      is_control_independent env p
    | Decl {body=p; var=x; _} ->
      is_control_independent (Variable.Set.remove x env) p
    | Loop (r, p) ->
      typecheck_r env r &&
      is_control_independent (Variable.Set.add (Range.var r) env) p
    | Acc _ -> true

  let rec is_data_independent (env:Variable.Set.t) : t -> bool =
    function
    | Cond (_, p) ->
      is_data_independent env p
    | Decl {var=x; body=p; _} ->
      is_data_independent (Variable.Set.remove x env) p
    | Loop (r, p) ->
      let env =
        if typecheck_r env r then
          (* if range is independent, then loop var is independent *)
          Variable.Set.add (Range.var r) env
        else
          env
      in
      is_data_independent env p
    | Acc (_, a) ->
      typecheck_a env a
end

let is_data_independent (env:Variable.Set.t) (p: Proto.Code.t) : bool =
  p
  |> Context.from_proto
  |> Seq.fold_left (fun is_di c ->
    is_di && Context.is_data_independent env c
  ) true

let is_control_independent (env:Variable.Set.t) (p: Proto.Code.t) : bool =
  p
  |> Context.from_proto
  |> Seq.fold_left (fun is_ci c ->
    is_ci && Context.is_control_independent env c
  ) true
