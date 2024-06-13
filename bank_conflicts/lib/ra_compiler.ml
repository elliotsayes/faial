open Stage0
open Protocols

type t =
  | Exact
  | Approximate

module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module L = Linearize_index.Make(L)

  let from_access_context
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
  :
    Bank.t -> Ra.Stmt.t
  =
    let rec from (locals:Variable.Set.t) : Bank.Code.t -> Ra.Stmt.t =
      function
      | Index a -> Tick (idx_analysis locals a)
      | Cond (_, p) -> from locals p
      | Decl (x, p) -> from (Variable.Set.add x locals) p
      | Loop (r, p) -> Loop (r, from locals p)
    in
    fun k ->
      from k.local_variables k.code

  let from_kernel
    ?(strategy=Exact)
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
    (cfg:Config.t)
    (k: Proto.Code.t Proto.Kernel.t)
  :
    Ra.Stmt.t
  =
    let if_ : Exp.bexp -> Ra.Stmt.t -> Ra.Stmt.t -> Ra.Stmt.t =
      match strategy with
      | Exact -> Ra.Stmt.Opt.if_
      | Approximate -> fun _ -> Ra.Stmt.Opt.choice
    in
    let lin = L.linearize cfg k.arrays in
    let rec from_p (locals:Variable.Set.t) : Proto.Code.t -> Ra.Stmt.t =
      function
      | Skip -> Skip
      | Seq (p, q) -> Seq (from_p locals p, from_p locals q)
      | Acc (x, {index=l; _}) ->
        l
        |> lin x
        |> Option.map (fun e ->
            let tick = idx_analysis locals e in
            Ra.Stmt.Tick tick
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
          if_ b p q
        else
          Seq (p, q)
      | Loop (r, p) ->
        let (r, p) =
          match R.uniform cfg.block_dim r with
          | Some r ->
            (* we need to invalidate the loop variable *)
            (r, from_p (Variable.Set.add (Range.var r) locals) p)
          | None -> (r, from_p locals p)
        in
        Loop (r, p)
    in
    k.code
    |> Proto.Code.subst_block_dim cfg.block_dim
    |> Proto.Code.subst_grid_dim cfg.grid_dim
    |> from_p (Params.to_set k.local_variables)
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
