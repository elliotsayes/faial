open Stage0
open Protocols

type t = {
  strategy: Analysis_strategy.t;
  locals: Variable.Set.t;
  index: Exp.nexp;
  divergence: Exp.bexp;
  config: Config.t;
}

module UA = struct
  type t =
    | Constant
    | Uniform
    | AnyAccurate
    | Inc

  let max (x1:t) (x2:t) : t =
    match x1, x2 with
    | Constant, e | e, Constant -> e
    | Uniform, e | e, Uniform -> e
    | AnyAccurate, e | e, AnyAccurate -> e
    | Inc, Inc -> Inc

  let bin : N_binary.t -> (Exp.nexp * t) -> (Exp.nexp * t) -> (Exp.nexp * t) =
    fun o (e1, x1) (e2, x2) ->
      let both : Exp.nexp = Binary (o, e1, e2) in
      if x1 = x2 then both, x1
      else if max x1 x2 = Uniform then
        both, max x1 x2
      else if (o = Plus || o = Minus) && (x1 = Uniform || x2 = Uniform) then
        (if x1 = Uniform then e2 else e1), Inc
      else
        both, max x1 x2

  let map (f:Exp.nexp -> Exp.nexp) ((e,x): Exp.nexp * t) : Exp.nexp * t =
    f e, x

  let from_nexp (cfg:Config.t) (locals:Variable.Set.t) : Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t =
      function
      | Num n -> Num n, Constant
      | Var x ->
        let r =
          if Config.is_warp_uniform x cfg then
            Uniform
          else if Variable.Set.mem x locals then
            AnyAccurate
          else
            Uniform
        in
        Var x, r
      | Unary (o, e) ->
        map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (o, e1, e2) ->
        bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, e) ->
        map (fun e -> NCall (f, e)) (from_nexp e)
      | Other e ->
        map (fun e -> Other e) (from_nexp e)
      | CastInt e ->
        let r =
          if Exp.b_intersects locals e then
            AnyAccurate
          else
            Uniform
        in
        CastInt e, r
      | NIf (c, e1, e2) ->
        if Exp.b_intersects locals c then
          NIf (c, e1, e2), Inc
        else
          let (e1, r1) = from_nexp e1 in
          let (e2, r2) = from_nexp e2 in
          NIf (c, e1, e2), max r1 r2
    in
    from_nexp

  let to_string : Exp.nexp * t -> string =
    fun (e, x) ->
      let prefix =
        match x with
        | Constant -> "num"
        | AnyAccurate -> "accurate"
        | Inc -> "inc"
        | Uniform -> "unif"
      in
      Exp.n_to_string e ^ ": " ^ prefix

end

module BC = struct
  (*
    Given a numeric expression try to remove any offsets in the form of
    `expression + constant` or `expression - constant`.

    The way we do this is by first getting all the free-names that are
    **not** tids. Secondly, we rearrange the expression as a polynomial
    in terms of each free variable. Third, we only keep polynomials that
    mention a tid, otherwise we can safely discard such a polynomial.
  *)
  type t =
    | Uniform
    | Any

  let bin : N_binary.t -> (Exp.nexp * t) -> (Exp.nexp * t) -> (Exp.nexp * t) =
    fun o (e1, x1) (e2, x2) ->
      let both : Exp.nexp = Binary (o, e1, e2) in
      match o, x1, x2 with
      | (Plus | Minus), Any, Uniform -> e1, Any
      | (Plus | Minus), Uniform, Any -> e2, Any
      | _, Uniform, Uniform -> both, Uniform
      | _, _, _ -> both, Any

  let map (f:Exp.nexp -> Exp.nexp) ((e,x): Exp.nexp * t) : Exp.nexp * t =
    f e, x

  let from_nexp (cfg:Config.t) (locals:Variable.Set.t) : Exp.nexp -> Exp.nexp * t =
    let locals = Variable.Set.union locals Variable.tid_set in
    let rec from_nexp : Exp.nexp -> Exp.nexp * t =
      function
      | Num n -> Num n, Uniform
      | Var x ->
        let r =
          if Config.is_warp_uniform x cfg then
            Uniform
          else if Variable.Set.mem x locals then
            Any
          else
            Uniform
        in
        Var x, r
      | Unary (o, e) ->
        map (fun e -> Unary (o, e)) (from_nexp e)
      | Binary (o, e1, e2) ->
        bin o (from_nexp e1) (from_nexp e2)
      | NCall (f, e) ->
        map (fun e -> NCall (f, e)) (from_nexp e)
      | Other e ->
        map (fun e -> Other e) (from_nexp e)
      | CastInt e ->
        let r =
          if Exp.b_intersects locals e then
            Any
          else
            Uniform
        in
        CastInt e, r
      | NIf (c, e1, e2) ->
        if Exp.b_intersects locals c then
          NIf (c, e1, e2), Any
        else
          let (e1, r1) = from_nexp e1 in
          let (e2, r2) = from_nexp e2 in
          let r =
            if r1 = r2 then
              r1
            else
              Any
          in
          NIf (c, e1, e2), r
    in
    from_nexp

  let to_string : Exp.nexp * t -> string =
    fun (e, x) ->
      let prefix =
        match x with
        | Any -> "any"
        | Uniform -> "unif"
      in
      Exp.n_to_string e ^ ": " ^ prefix
end

module Make (L:Logger.Logger) = struct
  open Exp

  let bc_remove_offset_aux
    (cfg:Config.t)
    (locals:Variable.Set.t) (index:Exp.nexp)
  : Exp.nexp =
    let after =
      match BC.from_nexp cfg locals index with
      | _, Uniform -> Num 0
      | e, Any -> e
    in
    (if index <> after then
      L.info ("BC: removed offset: " ^ Exp.n_to_string index ^ " 🡆 " ^ Exp.n_to_string after)
    );
    after

  let bc_remove_offset (ctx:t) : Exp.nexp =
    bc_remove_offset_aux ctx.config ctx.locals ctx.index

  let to_vectorized (ctx:t) : Vectorized.t =
    let vec = Vectorized.from_config ctx.config in
    if Result.is_ok (Vectorized.b_eval_res ctx.divergence vec) then
      Vectorized.restrict ctx.divergence vec
    else (
      L.info (
        "Index analysis: ignoring divergence: " ^ Exp.b_to_string ctx.divergence
      );
      vec
    )

  let run_bc
    (ctx:t)
  :
    Cost.t
  =
    let vec = to_vectorized ctx in
    let index = bc_remove_offset ctx in
    match Vectorized.to_cost Metric.BankConflicts index vec with
    | Ok cost -> cost
    | Error msg ->
      L.info ("BC: could not simulate cost " ^ Exp.n_to_string index ^ ": " ^ msg);
      Vectorized.max_cost Metric.BankConflicts vec

  let run_ua (ctx:t) : Cost.t =
    let vec = to_vectorized ctx in
    let (index, ty) = UA.from_nexp ctx.config ctx.locals ctx.index in
    (if ctx.index <> index then
      L.info (Printf.sprintf
        "UA: removed offset: %s 🡆 %s"
        (Exp.n_to_string ctx.index)
        (Exp.n_to_string index)
      )
    );
    if ty = UA.Uniform || ty = UA.Constant then (
      L.info ("UA: found a warp-uniform uncoalesced access" ^
        Exp.n_to_string ctx.index);
      Cost.from_int ~value:Metric.min_uncoalesced_accesses ~exact:true ()
    ) else
    match Vectorized.to_cost UncoalescedAccesses index vec with
    | Ok cost ->
      if ty = UA.Inc then (
        L.info ("UA: incrementing approximated cost: " ^ Cost.to_string cost);
        let v =
          min
            (cost.value + 1)
            (Vectorized.max_cost UncoalescedAccesses vec |> Cost.value)
        in
        Cost.set_value v cost
      ) else
        cost
    | Error msg ->
      L.info ("UA: could not simulate cost " ^ Exp.n_to_string index ^ ": " ^ msg);
      Vectorized.max_cost UncoalescedAccesses vec

  let run_count
    (_ctx:t)
  :
    Cost.t
  =
    Cost.from_int ~value:1 ~exact:true ()

  let run (m:Metric.t) (config:Config.t)
    ~strategy
    ~locals
    ~index
    ~divergence
  :
    Cost.t
  =
    let run =
      match m with
      | BankConflicts -> run_bc
      | UncoalescedAccesses -> run_ua
      | CountAccesses -> run_count
    in
    run {config; divergence; strategy; locals; index}
end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
