open Protocols
(*
  Given an arithmetic expression perform index analysis that yields the
  number of bank conflicts:
    1. remove any offsets that exist, ex `10 + tid` becomes `tid`
    2. evaluate any expression with constants and tids
  *)

(* Given a numeric expression try to remove any offsets in the form of
    `expression + constant` or `expression - constant`.

    The way we do this is by first getting all the free-names that are
    **not** tids. Secondly, we rearrange the expression as a polynomial
    in terms of each free variable. Third, we only keep polynomials that
    mention a tid, otherwise we can safely discard such a polynomial.
    *)
let remove_offset (fvs: Variable.Set.t) (n: Exp.nexp) : Exp.nexp =
  let rec rm_offset (n: Exp.nexp) : Variable.t list -> Exp.nexp =
    function
    | x :: fvs ->
      print_endline ("Removing offset variable '" ^ Variable.name x ^ "' from: " ^ Serialize.PPrint.n_to_s n);
      Poly.from_nexp x n
      (* We only want to keep polynomials that mention tid *)
      |> Poly.N.filter (fun coef _ ->
        Freenames.free_names_nexp coef Variable.Set.empty
        |> Variable.contains_tids
      )
      (* Recurse to remove any other constant factor mentioning fvs *)
      |> Poly.map1 (fun n ->
        rm_offset n fvs
      )
      (* Now convert back to a numeric expression *)
      |> Poly.to_nexp x

    | [] -> n
  in
  if Variable.Set.cardinal fvs > 0 then
    let n = rm_offset n (Variable.Set.elements fvs) in
    print_endline ("Expression without offsets: " ^ Serialize.PPrint.n_to_s n);
    n
  else n

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
      reason (Serialize.PPrint.n_to_s n);
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
    let fvs_minus_tids = Variable.Set.diff fvs Variable.tid_var_set in
    let n = remove_offset fvs_minus_tids n in
    try
      (Vectorized.access n ctx |> Vectorized.NMap.max).value - 1
    with
      Failure _ ->
      bc_fail "Could not analyze expression"
