open Stage0
open Protocols

type t =
  | Skip
  | Tick of int
  | Loop of Range.t * t
  | Seq of t * t
  | If of Exp.bexp * t * t

let rec is_zero : t -> bool =
  function
  | Skip
  | Tick 0 -> true
  | Tick _ -> false
  | Loop (_, r) -> is_zero r
  | Seq (p, q) -> is_zero p && is_zero q
  | If (_, p, q) -> is_zero p && is_zero q

let to_environ (s:t) : Environ.t =
  let rec fvs (s:t) (env:Environ.Fvs.t) : Environ.Fvs.t =
    match s with
    | Skip
    | Tick _ -> env
    | Loop (r, p) ->
      env
      |> fvs p
      |> Environ.Fvs.add_r r
    | Seq (p, q) ->
      env
      |> fvs p
      |> fvs q
    | If (b, p, q) ->
      env
      |> fvs p
      |> fvs q
      |> Environ.Fvs.add_b b
  in
  Environ.Fvs.empty
  |> fvs s
  |> Environ.from_fvs

module MakeSubst (S:Subst.SUBST) = struct
  module M = Subst.Make(S)
  let rec subst (s: S.t) : t -> t =
    function
    | Skip -> Skip
    | Tick k -> Tick k
    | Seq (p, q) -> Seq (subst s p, subst s q)
    | If (b, p, q) -> If (M.b_subst s b, subst s p, subst s q)
    | Loop (r, p) ->
      let r = M.r_subst s r in
      M.add s r.var (function
        | Some s -> Loop (r, subst s p)
        | None -> Loop (r, p)
      )
end

module PSubstAssoc = MakeSubst(Subst.SubstAssoc)
module PSubstPair = MakeSubst(Subst.SubstPair)

let subst : (Variable.t * Exp.nexp) -> t -> t = PSubstPair.subst

let rec indent : t -> Indent.t list =
  let open Indent in
  function
  | Tick k -> [Line ("tick(" ^ string_of_int k ^ ");")]
  | Skip -> [Line "skip;"]
  | Seq (p, q) -> indent p @ indent q
  | If (b, p, q) ->
    [
      Line ("if (" ^ Exp.b_to_string b ^ ") {");
      Block (indent p);
      Line ("} else {");
      Block (indent q);
      Line ("}")
    ]
  | Loop (r, p) ->
    [
      Line ("foreach (" ^ Range.to_string r ^ ") {");
      Block (indent p);
      Line "}"
    ]

module Opt = struct
  let skip : t = Skip

  let tick n : t =
    if n = 0 then Skip
    else Tick n

  let if_ (b:Exp.bexp) (p:t) (q:t) : t =
    match b, p, q with
    | Bool b, _, _ -> if b then p else q
    | _, Skip, Skip -> Skip
    | _, Skip, _ -> If (Exp.b_not b, p, Skip)
    | _, _, _ -> If (b, p, q)

  let seq (p:t) (q:t) : t =
    match p, q with
    | Skip, p | p, Skip -> p
    | _, _ -> Seq (p, q)

  let loop (r:Range.t) (p:t) : t =
    if p = Skip || Range.eval_is_empty r then
      Skip
    else
      Loop (r, p)
end

let rec simplify : t -> t =
  function
  | Skip -> Skip
  | Tick n -> Opt.tick n
  | If (b, p, q) ->
    Opt.if_ (Constfold.b_opt b) (simplify p) (simplify q)
  | Loop (r,p) ->
    Opt.loop (Constfold.r_opt r) (simplify p)
  | Seq (p, q) ->
    Opt.seq (simplify p) (simplify q)

let to_string (x:t) : string =
  indent x |> Indent.to_string

let print (x:t) : unit =
  indent x |> Indent.print

module Make (L:Logger.Logger) = struct
  module R = Uniform_range.Make(L)
  module L = Linearize_index.Make(L)

  let from_access_context
    (idx_analysis : Variable.Set.t -> Exp.nexp -> int)
  :
    Variable.Set.t -> Access_context.t -> t
  =
    let rec from (locals:Variable.Set.t) : Access_context.t -> t =
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
    t
  =
    let lin = L.linearize cfg k.arrays in
    let rec from_p (locals:Variable.Set.t) : Proto.Code.t -> t =
      function
      | Skip -> Skip
      | Seq (p, q) -> Seq (from_p locals p, from_p locals q)
      | Acc (x, {index=l; _}) ->
        l
        |> lin x
        |> Option.map (fun e ->
            Tick (idx_analysis locals e)
          )
        |> Option.value ~default:Skip
      | Sync _ -> Skip
      | Decl {body=p; var; _} -> from_p (Variable.Set.add var locals) p
      | Cond (b, p) ->
        let fns =
          Variable.Set.inter
            (Freenames.free_names_bexp b Variable.Set.empty)
            (Variable.Set.union locals Variable.tid_var_set)
        in
        if Variable.Set.is_empty fns then
          If (b, from_p locals p, Skip)
        else
          from_p locals p
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
