open Stage0
open Protocols

(*
  Given a numeric expression try to remove any offsets in the form of
  `expression + constant` or `expression - constant`.

  The way we do this is by first getting all the free-names that are
  **not** tids. Secondly, we rearrange the expression as a polynomial
  in terms of each free variable. Third, we only keep polynomials that
  mention a tid, otherwise we can safely discard such a polynomial.
*)

module UA = struct
  type t =
    | Constant
    | Uniform
    | AnyAccurate
    | AnyApprox

  let max (x1:t) (x2:t) : t =
    match x1, x2 with
    | Constant, e | e, Constant -> e
    | Uniform, e | e, Uniform -> e
    | AnyAccurate, e | e, AnyAccurate -> e
    | AnyApprox, AnyApprox -> AnyApprox

  let bin : N_binary.t -> (Exp.nexp * t) -> (Exp.nexp * t) -> (Exp.nexp * t) =
    fun o (e1, x1) (e2, x2) ->
      let both : Exp.nexp = Binary (o, e1, e2) in
      if x1 = x2 then both, x1
      else if (o = Plus || o = Minus) && (x1 = Uniform || x2 = Uniform) then
        (if x1 = Uniform then e2 else e1), AnyApprox
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
          NIf (c, e1, e2), AnyApprox
        else
          let (e1, r1) = from_nexp e1 in
          let (e2, r2) = from_nexp e2 in
          NIf (c, e1, e2), max r1 r2
    in
    from_nexp

end

module BC = struct
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

  let bc_remove_offset (cfg:Config.t) (locals:Variable.Set.t) (n: Exp.nexp) : Exp.nexp =
    let after =
      match BC.from_nexp cfg locals n with
      | _, Uniform -> Num 0
      | e, Any -> e
    in
    (if n <> after then
      L.info ("BC: removed offset: " ^ Exp.n_to_string n ^ " 🡆 " ^ Exp.n_to_string after)
    );
    after

  let run_bc
    (cfg:Config.t)
    (locals:Variable.Set.t)
    (e: Exp.nexp)
  :
    (Cost.t, string) Result.t
  =
    let ctx = Vectorized.from_config cfg in
    let e = bc_remove_offset cfg locals e in
    Vectorized.to_cost Metric.BankConflicts e ctx

  let run_ua
    (cfg:Config.t)
    (locals:Variable.Set.t)
    (e: Exp.nexp)
  :
    (Cost.t, string) Result.t
  =
    let ctx = Vectorized.from_config cfg in
    let (new_e, ty) = UA.from_nexp cfg locals e in
    (if e <> new_e then
      L.info ("UA: removed offset: " ^ Exp.n_to_string e ^ " 🡆 " ^ Exp.n_to_string new_e)
    );
    Vectorized.to_cost Metric.UncoalescedAccesses new_e ctx
    |> Result.map (fun c ->
        if ty = UA.AnyApprox then
          let open Cost in
          L.info ("UA: incrementing approximated cost: " ^ Cost.to_string c);
          { c with
            value =
              min
                (c.value + 1)
                (Metric.max_uncoalesced_accesses cfg)
          }
        else
          c
      )

  let run_count
    (_cfg:Config.t)
    (_locals:Variable.Set.t)
    (_e: Exp.nexp)
  :
    (Cost.t, string) Result.t
  =
    Ok (Cost.from_int 1)

  let run :
    Metric.t ->
    Config.t ->
    Variable.Set.t ->
    Exp.nexp ->
    (Cost.t, string) Result.t
  =
    function
    | BankConflicts -> run_bc
    | UncoalescedAccesses -> run_ua
    | CountAccesses -> run_count

end
module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
