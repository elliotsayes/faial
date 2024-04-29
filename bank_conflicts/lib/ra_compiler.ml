open Stage0
open Protocols

module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module L = Linearize_index.Make(L)

  let from_access_context
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
  :
    Variable.Set.t -> Access_context.t -> Ra.Stmt.t
  =
    let rec from (locals:Variable.Set.t) : Access_context.t -> Ra.Stmt.t =
      function
      | Index a -> Tick (idx_analysis locals a.index)
      | Cond (_, p) -> from locals p
      | Decl (x, p) -> from (Variable.Set.add x locals) p
      | Loop (r, p) -> Loop (r, from locals p)
    in
    from

  let from_kernel
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
    (cfg:Config.t)
    (k: Proto.Code.t Proto.Kernel.t)
  :
    Ra.Stmt.t
  =
    let lin = L.linearize cfg k.arrays in
    let rec from_p (locals:Variable.Set.t) : Proto.Code.t -> Ra.Stmt.t =
      function
      | Skip -> Skip
      | Seq (p, q) -> Seq (from_p locals p, from_p locals q)
      | Acc (x, {index=l; _}) ->
        l
        |> lin x
        |> Option.map (fun e ->
            Ra.Stmt.Tick (idx_analysis locals e)
          )
        |> Option.value ~default:Ra.Stmt.Skip
      | Sync _ -> Skip
      | Decl {body=p; var; _} -> from_p (Variable.Set.add var locals) p
      | If (b, p, q) ->
        let fns =
          Variable.Set.inter
            (Freenames.free_names_bexp b Variable.Set.empty)
            (Variable.Set.union locals Variable.tid_var_set)
        in
        let p = from_p locals p in
        let q = from_p locals q in
        if Variable.Set.is_empty fns then
          If (b, p, q)
        else
          Seq (p, q)
      | Loop (r, p) ->
        let r = R.uniform cfg.block_dim r |> Option.value ~default:r in
        Loop (r, from_p locals p)
    in
    k.code
    |> Proto.Code.subst_block_dim cfg.block_dim
    |> Proto.Code.subst_grid_dim cfg.grid_dim
    |> from_p (Params.to_set k.local_variables)
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
