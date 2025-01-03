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

module Approx = struct
  type t = {exact_index: bool; exact_loop: bool; exact_condition: bool}

  let exact : t =
    { exact_index = true; exact_loop = true; exact_condition = true}

  let set_exact_index (e:bool) (a:t) : t =
    { a with exact_index = e }

  let add (lhs:t) (rhs:t) : t =
    {
      exact_index = lhs.exact_index && rhs.exact_index;
      exact_loop = lhs.exact_loop && rhs.exact_loop;
      exact_condition = lhs.exact_condition && rhs.exact_condition;
    }

  let set_unexact_cond (e:t) : t =
    { e with exact_index = false }

  let set_unexact_loop (e:t) : t =
    { e with exact_loop = false }

  let to_string (e:t) : string =
    let f =
      function
      | true -> "exact"
      | false -> "inexact"
    in
    Printf.sprintf
      "{index=%s, loop=%s, cond=%s}"
      (f e.exact_index)
      (f e.exact_loop)
      (f e.exact_condition)
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
    (idx_analysis : Variable.Set.t -> Exp.nexp -> Cost.t)
    (cfg:Config.t)
    (k:Kernel.t)
  :
    (Ra.Stmt.t * Approx.t, string) Result.t
  =
    let ( let* ) = Result.bind in
    let if_ : Exp.bexp -> Ra.Stmt.t -> Ra.Stmt.t -> Ra.Stmt.t =
      match unif_cond with
      | Exact -> Ra.Stmt.Opt.if_
      | Approximate -> fun _ -> Ra.Stmt.Opt.choice
    in
    let lin = L.linearize cfg k.arrays in
    let params = k.global_variables in
    let rec from_p (approx:Approx.t) (locals:Variable.Set.t)
    :
      Code.t -> (Ra.Stmt.t * Approx.t, string) Result.t
    =
      let open Ra.Stmt in
      function
      | Skip -> Ok (Skip, Approx.exact)
      | Seq (p, q) ->
        let* (p, approx1) = from_p approx locals p in
        let* (q, approx2) = from_p approx locals q in
        Ok (Seq (p, q), Approx.add approx1 approx2)
      | Access {array=x; index=l; _} ->
        Ok (
          l
          |> lin x (* Returns None when the array is being ignored *)
          |> Option.map (fun e ->
              let tick = idx_analysis locals e in
              (
                Ra.Stmt.Tick (Cost.value tick),
                Approx.set_exact_index tick.exact approx
              )
            )
            (* When the array is ignored, return Skip *)
          |> Option.value ~default:(Ra.Stmt.Skip, Approx.exact)
        )
      | Sync _ -> Ok (Skip, Approx.exact)
      | Decl {body=p; var; _} ->
        from_p approx (Variable.Set.add var locals) p
      | If (b, p, q) ->
        let fns =
          Variable.Set.inter
            (Exp.b_free_names b Variable.Set.empty)
            locals
        in
        let* (p, approx1) = from_p approx locals p in
        let* (q, approx2) = from_p approx locals q in
        let approx = Approx.add approx1 approx2 in
        Ok (
          (* Warp-uniform *)
          if Variable.Set.is_empty fns then
            (if_ b p q, approx)
          (* Warp-divergent *)
          else
            let p =
              match divergence with
              | Over ->
                Seq (p, q)
              | Under ->
                Skip
            in
            (p, Approx.set_unexact_cond approx)
        )
      | Loop {range; body} ->
        let free_locals =
          Range.free_names range Variable.Set.empty
          |> Variable.Set.inter locals
        in
        let only_tid_in_locals =
          Variable.Set.diff free_locals Variable.tid_set
          |> Variable.Set.is_empty
        in
        (* Warp-uniform loop *)
        if Variable.Set.is_empty free_locals then
          let* (body, approx) = from_p approx locals body in
          Ok (Loop {range; body;}, approx)
        (* Warp-divergent loop *)
        else if only_tid_in_locals then (
          (* get the first number *)
          let f = Range.first range in
          let (range, locals) =
            let strat = Divergence.to_optimize divergence in
            match R.uniform strat params cfg.block_dim range with
            | Some range ->
              let locals =
                (*
                  If the first element of the range has a tid, then
                  the loop variable should be considered a thread-local.
                  Otherwise, the loop variable can be considered
                  thread-global.
                *)
                if Exp.n_exists Variable.is_tid f then
                  Variable.Set.add (Range.var range) locals
                else
                  locals
              in
              (* we need to invalidate the loop variable *)
              (range, locals)
            | None ->
              (range, locals)
          in
          let* (body, approx) = from_p approx locals body in
          Ok (Loop {range; body}, Approx.set_unexact_loop approx)
        ) else if divergence = Under then
          Ok (Skip, Approx.set_unexact_loop approx)
          (* When there are only tids in the set of local names,
            then we maximize the loop *)
        else (* maximizing, just leads to an arbitrary loop *)
          let* (body, approx) =
            from_p approx (Variable.Set.add (Range.var range) locals) body
          in
          (* At this point the loop bounds are unknown, yet the body
             may produce 0 ticks, in which case, we can just eagerly
             collapse the loop. *)
          if Ra.Stmt.is_zero body then
            Ok (Skip, Approx.set_unexact_loop approx)
          else
            (* Finally, we get to a point where the loop bounds are
               thread-local and we know nothing about them. *)
            Error ("Unsupported loop range: " ^ Range.to_string range)
    in
    let locals =
      Params.to_set k.local_variables
      |> Variable.Set.union Variable.tid_set
    in
    k.code
    |> Code.subst_block_dim cfg.block_dim
    |> Code.subst_grid_dim cfg.grid_dim
    |> from_p Approx.exact locals
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
