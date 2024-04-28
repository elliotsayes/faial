open Protocols
open Stage0

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

  let rec location : t -> Location.t =
    function
    | Cond (_, l) | Decl {body=l; _} | Loop (_, l) ->
      location l
    | Acc (x, _) ->
      Variable.location x

  let rec from_code : Proto.Code.t -> t Seq.t =
    function
    | Acc (x, y) -> Seq.return (Acc (x, y))
    | Sync _ | Skip -> Seq.empty
    | If (b, p, q) ->
      Seq.append
        (from_code p |> Seq.map (fun p -> Cond (b, p)))
        (from_code q |> Seq.map (fun q -> Cond (Exp.b_not b, q)))
    | Loop (r, p) ->
      from_code p |> Seq.map (fun c -> Loop (r, c))
    | Seq (p, q) ->
      from_code p |> Seq.append (from_code q)
    | Decl {var; ty; body} ->
      from_code body |> Seq.map (fun body -> Decl {var; ty; body})

  let from_kernel (k: Proto.Code.t Proto.Kernel.t) : t Seq.t =
    If (k.pre, k.code, Skip)
    |> from_code

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

let kernel_env (k: Proto.Code.t Proto.Kernel.t) : Variable.Set.t =
  let globals = Params.to_set k.global_variables in
  Variable.Set.union globals Variable.tid_var_set

type t = {data_independent: bool; control_independent: bool}

let from_context (env:Variable.Set.t) (c:Context.t) : t =
  {
    data_independent = Context.is_data_independent env c;
    control_independent = Context.is_control_independent env c;
  }

let add (env:Variable.Set.t) (r:t) (c:Context.t) : t =
  let data_independent =
    r.data_independent && Context.is_data_independent env c
  in
  let control_independent =
    r.control_independent && Context.is_control_independent env c
  in
  {
    data_independent;
    control_independent;
  }

let ci_di : t = { control_independent=true; data_independent=true; }

let to_string (x:t) =
  let ci = if x.control_independent then "CI" else "CD" in
  let di = if x.data_independent then "DI" else "DD" in
  ci ^ di

let per_kernel (k: Proto.Code.t Proto.Kernel.t) : t =
  let env = kernel_env k in
  k
  |> Context.from_kernel
  |> Seq.fold_left (add env) ci_di

let per_access (k: Proto.Code.t Proto.Kernel.t) : (Context.t * t) list =
  let env = kernel_env k in
  k
  |> Context.from_kernel
  |> Seq.map (fun (c:Context.t) ->
    (c, from_context env c)
  )
  |> List.of_seq
