open Protocols
(*
  Given an arithmetic expression perform index analysis that yields the
  number of bank conflicts:
    1. remove any offsets that exist, ex `10 + tid` becomes `tid`
    2. evaluate any expression with constants and tids
  *)

module OffsetAnalysis = struct
  (* Given a numeric expression try to remove any offsets in the form of
    `expression + constant` or `expression - constant`.

    The way we do this is by first getting all the free-names that are
    **not** tids. Secondly, we rearrange the expression as a polynomial
    in terms of each free variable. Third, we only keep polynomials that
    mention a tid, otherwise we can safely discard such a polynomial.
    *)
  open Exp
  type t =
    | Offset of nexp
    | Index of nexp

  let map (f:nexp -> nexp) : t -> t =
    function
    | Offset e -> Offset (f e)
    | Index e -> Index (f e)

  let index_or (f:nexp -> nexp -> nexp) (e1: t) (e2: t) : t =
    match e1, e2 with
    | Index e, Offset _
    | Offset _, Index e -> Index e
    | Offset e1, Offset e2 -> Offset (f e1 e2)
    | Index e1, Index e2 -> Index (f e1 e2)

  let index_and (f:nexp -> nexp -> nexp) (e1: t) (e2: t) : t =
    match e1, e2 with
    | Index e1, Offset e2
    | Offset e1, Index e2
    | Index e1, Index e2 -> Index (f e1 e2)
    | Offset e1, Offset e2 -> Offset (f e1 e2)

  let rec from_nexp : Exp.nexp -> t =
    function
    | Num n -> Offset (Num n)
    | Var x when Variable.is_tid x -> Index (Var x)
    | Var x -> Offset (Var x)
    | Bin (o, e1, e2) when o = Plus || o = Minus ->
      index_or (fun e1 e2 -> Bin (o, e1, e2)) (from_nexp e1) (from_nexp e2)
    | Bin (o, e1, e2) ->
      index_and (fun e1 e2 -> Bin (o, e1, e2)) (from_nexp e1) (from_nexp e2)
    | Proj (p, e) -> Offset (Proj (p, e))
    | NCall (f, e) -> map (fun e -> NCall (f, e)) (from_nexp e)
    | NIf (c, n1, n2) ->
      if Freenames.contains_tid_bexp c then
        Index (NIf (c, n1, n2))
      else
        index_and (fun n1 n2 -> NIf (c, n1, n2)) (from_nexp n1) (from_nexp n2)

  let to_string : t -> string =
    function
    | Index e -> "index " ^ Exp.n_to_string e
    | Offset e -> "offset " ^ Exp.n_to_string e

  let remove_offset (n: Exp.nexp) : Exp.nexp =
    let after = match from_nexp n with
    | Offset _ -> Num 0
    | Index e -> e
    in
    (if n = after then () else
      prerr_endline ("WARNING: removed offset: " ^ Exp.n_to_string n ^ " 🡆 " ^ Exp.n_to_string after)
    );
    after
end

(*
  1. If the expressions contains any thread-local variable, return the max
      number of bank conflicts
  2. Remove any uniform offsets that appear in the expression
  3. Try to evaluate the expression, which will only work if the expression
      does _not_ contain any variables.
  4. Otherwise, return the max number of bank conflicts.
*)
let analyze (num_banks:int) (thread_count:Vec3.t) (thread_locals : Variable.Set.t) (n : Exp.nexp) : int =
  let bc_fail (reason : string) : int =
    Printf.eprintf
      "WARNING: %s: %s\n"
      reason (Exp.n_to_string n);
    num_banks
  in
  let thread_locals = Variable.Set.diff thread_locals Variable.tid_var_set in
  let fvs = Freenames.free_names_nexp n Variable.Set.empty in
  let has_thread_locals : bool =
    not (Variable.Set.inter thread_locals fvs |> Variable.Set.is_empty)
  in
  if has_thread_locals then
    bc_fail "Expression uses thread-local variables"
  else
    let ctx =
      let open Vectorized in
        make
        ~bank_count:num_banks
        ~warp_count:num_banks
        ~use_array:(fun _ -> true)
      |> put_tids thread_count
    in
    let n = OffsetAnalysis.remove_offset n in
    try
      (Vectorized.access n ctx |> Vectorized.NMap.max).value - 1
    with
      Failure _ ->
      bc_fail "Could not analyze expression"
