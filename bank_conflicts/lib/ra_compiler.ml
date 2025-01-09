open Stage0
open Protocols
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

  let is_thread_uniform (e:t) : bool =
    e.exact_loop && e.exact_condition

  let is_thread_divergent (e:t) : bool =
    not (is_thread_uniform e)

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

let to_optimize : Analysis_strategy.t -> Uniform_range.t =
  function
  | OverApproximation -> Uniform_range.Maximize
  | UnderApproximation -> Uniform_range.Minimize

module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module I = Index_analysis.Make(L)
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
    ?(strategy=Analysis_strategy.OverApproximation)
    (m:Metric.t)
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
    let idx_analysis = I.run m cfg ~strategy in
    let uniform_loop = R.uniform (to_optimize strategy) params cfg.block_dim in
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
          |> Option.map (fun index ->
              let tick =
                idx_analysis
                  ~thread_divergent:(Approx.is_thread_divergent approx)
                  ~locals
                  ~index
              in
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
              match strategy with
              | OverApproximation ->
                Seq (p, q)
              | UnderApproximation ->
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
          let init = Range.first range in
          let (range, locals) =
            match uniform_loop range with
            | Some range ->
              let locals =
                (*
                  If the first element of the range has a tid, then
                  the loop variable should be considered a thread-local.
                  Otherwise, the loop variable can be considered
                  thread-global.
                *)
                if Exp.n_exists Variable.is_tid init then
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
        ) else
          let* (body, _) = from_p approx locals body in
          if Ra.Stmt.is_zero body then Ok (Skip, Approx.exact) else
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
