open Proto

(** Given a variable and a set of known variables, returns
    a fresh variable name. *)

let generate_fresh_name (x:variable) (xs:VarSet.t) : variable =
  let rec do_fresh_name x n =
    let new_x = {x with var_name = x.var_name ^ string_of_int n } in
    if VarSet.mem new_x xs
    then do_fresh_name x (n + 1)
    else new_x
  in
  if VarSet.mem x xs then do_fresh_name x 1 else x

(** Loop normalization: Makes all loop variables distinct. *)
(*
let normalize_variables (p:prog) =
  let rec norm_inst (i:inst) (xs:VarSet.t) : inst * VarSet.t =
    match i with
    | Loop (r, p) ->
      if VarSet.mem r.range_var xs then (
        let new_x : variable = generate_fresh_name r.range_var xs in
        let new_xs = VarSet.add new_x xs in
        let do_subst = Subst.SubstPair.make (r.range_var, Var new_x) in
        let (p, new_xs) = norm (Subst.ReplacePair.p_subst do_subst p) new_xs in
        Loop ({r with range_var=new_x}, p), new_xs
      ) else (
        let (p, new_xs) = norm p (VarSet.add r.range_var xs) in
        Loop (r, p), new_xs
      )
    | Cond (b, p) ->
      let (p, xs) = norm p xs in
      (Cond (b, p), xs)
    | Base _ -> i, xs
  and norm p xs =
    match p with
    | [] -> ([], xs)
    | i::p ->
      let (i, xs) = norm_inst i xs in
      let (p, xs) = norm p xs in
      (i::p, xs)
  in
  norm p VarSet.empty |> fst
  *)