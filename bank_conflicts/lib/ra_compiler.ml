open Stage0
open Protocols
module Divergence = struct
  type t = Over | Under
  let to_optimize : t -> Uniform_range.t =
    function
    | Over -> Maximize
    | Under -> Minimize
end
module UniformCond = struct
  type t = Exact | Approximate
end

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
      | Loop {range; body} -> Loop {range; body=from locals body}
    in
    fun k ->
      from k.local_variables k.code

  let from_kernel
    ?(unif_cond=UniformCond.Exact)
    ?(divergence=Divergence.Over)
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
    (cfg:Config.t)
    (k:Kernel.t)
  :
    Ra.Stmt.t
  =
    let if_ : Exp.bexp -> Ra.Stmt.t -> Ra.Stmt.t -> Ra.Stmt.t =
      match unif_cond with
      | Exact -> Ra.Stmt.Opt.if_
      | Approximate -> fun _ -> Ra.Stmt.Opt.choice
    in
    let lin = L.linearize cfg k.arrays in
    let params = k.global_variables in
    let rec from_p (locals:Variable.Set.t) : Code.t -> Ra.Stmt.t =
      function
      | Skip -> Skip
      | Seq (p, q) -> Seq (from_p locals p, from_p locals q)
      | Access {array=x; index=l; _} ->
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
            (Exp.b_free_names b Variable.Set.empty)
            locals
        in
        let p = from_p locals p in
        let q = from_p locals q in
        (* Warp-uniform *)
        if Variable.Set.is_empty fns then
          if_ b p q
        (* Warp-divergent *)
        else if divergence = Over then
          Seq (p, q)
        else
          Skip
      | Loop {range=r; body=p} ->
        let free_locals =
          Range.free_names r Variable.Set.empty
          |> Variable.Set.inter locals
        in
        let only_tid_in_locals =
          Variable.Set.diff free_locals Variable.tid_set
          |> Variable.Set.is_empty
        in
        (* Warp-uniform loop *)
        if Variable.Set.is_empty free_locals then
          Loop {range=r; body=from_p locals p;}
        (* Warp-divergent loop *)
        else if only_tid_in_locals then (
          (* get the first number *)
          let f = Range.first r in
          let (r, p) =
            let strat = Divergence.to_optimize divergence in
            match R.uniform strat params cfg.block_dim r with
            | Some r ->
              let locals =
                (*
                  If the first element of the range has a tid, then
                  the loop variable should be considered a thread-local.
                  Otherwise, the loop variable can be considered
                  thread-global.
                *)
                if Exp.n_exists Variable.is_tid f then
                  Variable.Set.add (Range.var r) locals
                else
                  locals
              in
              (* we need to invalidate the loop variable *)
              (r, from_p locals p)
            | None -> (r, from_p locals p)
          in
          Loop {range=r; body=p}
      ) else if divergence = Under then
          Skip
        (* When there are only tids in the set of local names,
           then we maximize the loop *)
      else (* maximizing, just leads to an arbitrary loop *)
        (* Unsupported loop *)
        Loop {range=r; body=from_p (Variable.Set.add (Range.var r) locals) p}
    in
    let locals =
      Params.to_set k.local_variables
      |> Variable.Set.union Variable.tid_set
    in
    k.code
    |> Code.subst_block_dim cfg.block_dim
    |> Code.subst_grid_dim cfg.grid_dim
    |> from_p locals
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
