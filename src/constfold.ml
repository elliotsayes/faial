open Exp

let rec norm (b:bexp) : bexp list =
  match b with
  | Pred _
  | BNot (Pred _)
  | BRel (BOr, _, _)
  | Bool _
  | NRel _ -> [b]
  | BRel (BAnd, b1, b2) -> List.append (norm b1) (norm b2)
  | BNot (Bool b) -> [Bool (not b)]
  | BNot (BRel (BAnd, b1, b2)) -> norm (b_or (b_not b1) (b_not b2))
  | BNot (BRel (BOr, b1, b2)) -> norm (b_and (b_not b1) (b_not b2))
  | BNot (NRel (NEq, n1, n2)) -> norm (n_neq n1 n2)
  | BNot (NRel (NNeq, n1, n2)) -> norm (n_eq n1 n2)
  | BNot (NRel (NGt, n1, n2)) -> norm (n_ge n1 n2)
  | BNot (NRel (NLt, n1, n2)) -> norm (n_le n1 n2)
  | BNot (NRel (NLe, n1, n2)) -> norm (n_lt n1 n2)
  | BNot (NRel (NGe, n1, n2)) -> norm (n_gt n1 n2)
  | BNot (BNot b) -> norm b

let bexp_to_bool b =
  match b with
  | Bool b -> Some b
  | _ -> None

let rec n_opt (a : nexp) : nexp =
  match a with
  | Var _ -> a
  | Num n -> if n < 0 then raise (Failure "Negative number") else a
  | Proj (t, n) -> Proj(t, n)
  | NCall (x, e) ->
    begin match n_opt e with
    | Num _ as n ->
      (* Try to evaluate the predicate *)
      begin match Predicates.func_call_opt x n with
      | Some n ->  (* We found the predicate; call it and optimize the result *)
        n_opt n
      | None -> (* Otherwise, leave the predicate unchanged *)
        NCall (x, n)
      end
    | v -> NCall (x, v)
    end
  | NIf (b, n1, n2) ->
    let b = b_opt b in
    let n1 = n_opt n1 in
    let n2 = n_opt n2 in
    begin match b with
    | Bool true -> n1
    | Bool false -> n2
    | _ -> NIf (b, n1, n2)
    end
  | Bin (b, a1, a2) ->
    let a1 = n_opt a1 in
    let a2 = n_opt a2 in
    match b, a1, a2 with
    (* Absorb *)
    | Minus, Num 0, _
    | Mult, Num 0, _
    | Mult, _, Num 0
    | Div, Num 0, _
    | Mod, _, Num 1
    -> Num 0
    | Mod, _, Num 0
    | Div, _, Num 0
    -> raise (Failure "Division by zero")
    (* Neutral *)
    | Plus, Num 0, a
    | Plus, a, Num 0
    | Minus, a, Num 0
    | Div, a, Num 1
    | Mult, Num 1, a
    | Mult, a, Num 1
    -> a
    | LeftShift, a, Num n -> n_opt (n_mult a (Num (Predicates.pow 2 n)))
    | RightShift, a, Num n -> n_opt (n_div a (Num (Predicates.pow 2 n)))
    (* Compute *)
    | _, Num n1, Num n2 -> Num ((eval_nbin b) n1 n2)
    (* Propagate *)
    | _, _, _ -> Bin (b, a1, a2)

and b_opt (e : bexp) : bexp =
  match e with
  | Pred (x, e) ->
    begin match n_opt e with
    | Num _ as n ->
      (* Try to evaluate the predicate *)
      begin match Predicates.pred_call_opt x n with
      | Some b ->  (* We found the predicate; call it and optimize the result *)
        b_opt b
      | None -> (* Otherwise, leave the predicate unchanged *)
        Pred (x, n)
      end
    | v -> Pred (x, v)
    end
  | Bool _ -> e
  | BRel (b, b1, b2) ->
    begin
      let b1 = b_opt b1 in
      let b2 = b_opt b2 in
      match b, bexp_to_bool b1, bexp_to_bool b2 with
      | _, Some b1, Some b2 -> Bool ((eval_brel b) b1 b2)
      | BAnd, _, Some true -> b1
      | BAnd, Some true, _ -> b2
      | BAnd, Some false, _
      | BAnd, _, Some false
        -> Bool false
      | BOr, Some true, _
      | BOr, _, Some true
        -> Bool true
      | BOr, _, Some false -> b1
      | BOr, Some false, _ -> b2
      | _, _, _ -> BRel (b, b1, b2)
    end
  | NRel (o, a1, a2) ->
    begin
      let a1 = n_opt a1 in
      let a2 = n_opt a2 in
      match a1, a2 with
      | Num n1, Num n2 -> Bool ((eval_nrel o) n1 n2)
      | _, _ -> NRel (o, a1, a2)
    end
  | BNot b ->
    let b = b_opt b in
    match bexp_to_bool b with
    | Some b -> Bool (not b)
    | _ -> BNot b

let r_opt (r:range) : range =
  {
    r with
    range_lower_bound = n_opt r.range_lower_bound;
    range_upper_bound = n_opt r.range_upper_bound;
  }

let a_opt a =
  {
    access_mode = a.access_mode;
    access_index = List.map n_opt a.access_index;
  }
