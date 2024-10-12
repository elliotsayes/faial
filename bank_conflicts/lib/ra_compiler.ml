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
    (k:Kernel.t)
  :
    Ra.Stmt.t
  =
    let if_ : Exp.bexp -> Ra.Stmt.t -> Ra.Stmt.t -> Ra.Stmt.t =
      match strategy with
      | Exact -> Ra.Stmt.Opt.if_
      | Approximate -> fun _ -> Ra.Stmt.Opt.choice
    in
    let lin = L.linearize cfg k.arrays in
    let params = k.global_variables in
    let rec from_p (locals:Variable.Set.t) : Code.t -> Ra.Stmt.t =
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
            (Exp.b_free_names b Variable.Set.empty)
            locals
        in
        let p = from_p locals p in
        let q = from_p locals q in
        if Variable.Set.is_empty fns then
          if_ b p q
        else
          Seq (p, q)
      | Loop (r, p) ->
        let free_locals =
          Range.free_names r Variable.Set.empty
          |> Variable.Set.inter locals
        in
        let only_tid_in_locals =
          Variable.Set.diff free_locals Variable.tid_set
          |> Variable.Set.is_empty
        in
        if Variable.Set.is_empty free_locals then
          (* Uniform loop *)
          Loop (r, from_p locals p)
        else if not only_tid_in_locals then
          (* Unsupported loop *)
          Loop (r, from_p (Variable.Set.add (Range.var r) locals) p)
        else
        (* get the first number *)
        let f = Range.first r in
        let (r, p) =
          match R.uniform params cfg.block_dim r with
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
        Loop (r, p)
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
