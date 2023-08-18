open Exp
open Stage0

let rec norm (b:bexp) : bexp list =
  match b with
  | ThreadEqual _
  | Pred _
  | BNot (Pred _)
  | BRel (BOr, _, _)
  | Bool _
  | NRel _ -> [b]
  | BRel (BAnd, b1, b2) -> List.append (norm b1) (norm b2)
  | BNot (ThreadEqual _) -> [b]
  | BNot (Bool b) -> [Bool (not b)]
  | BNot (BRel (BAnd, b1, b2)) -> norm (b_or (b_not b1) (b_not b2))
  | BNot (BRel (BOr, b1, b2)) -> norm (b_and (b_not b1) (b_not b2))
  | BNot (NRel (NEq, n1, n2)) -> norm (n_neq n1 n2)
  | BNot (NRel (NNeq, n1, n2)) -> norm (n_eq n1 n2)
  | BNot (NRel (NGt, n1, n2)) -> norm (n_le n1 n2)
  | BNot (NRel (NLt, n1, n2)) -> norm (n_ge n1 n2)
  | BNot (NRel (NLe, n1, n2)) -> norm (n_gt n1 n2)
  | BNot (NRel (NGe, n1, n2)) -> norm (n_lt n1 n2)
  | BNot (BNot b) -> norm b

let bexp_to_bool b =
  match b with
  | Bool b -> Some b
  | _ -> None

let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

let rec n_opt (a : nexp) : nexp =
  match a with
  | Var _ 
  | Num _ -> a
    (* if n < 0 then raise (Failure "Negative number") else a *)
  | NCall (x, e) -> NCall (x, n_opt e)
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
    | _, Num a1, Num a2 ->
      Num (eval_nbin b a1 a2)
    (* Absorb *)
    | Mult, Num 0, _
    | Mult, _, Num 0
    | Mod, _, Num 1
    -> Num 0
    | Mod, _, Num 0
    -> raise (Failure ("Division by zero: " ^ n_to_string a))
    (* Neutral *)
    | Plus, Num 0, a
    | Plus, a, Num 0
    | Minus, a, Num 0
    | Div, a, Num 1
    | Mult, Num 1, a
    | Mult, a, Num 1
    -> a
    | LeftShift, a, Num n -> n_opt (Bin (Mult, a, Num (Common.pow ~base:2 n)))
    | RightShift, a, Num n -> n_opt (Bin (Div, a, Num (Common.pow ~base:2 n)))
    (* Compute *)
      (*

      e1   e2   n2*e1 + n1*e2
      -- + -- = -------------
      n1   n2    n1*n2

       *)
(*    | Mult, Bin (Div, Num n1, Num n2), Bin (Div, Num n3, Num n4) ->
      n_opt (Bin (Div, Num (n1*n3), Num (n2*n4)))
    | Plus, Bin (Div, e1, Num n1), Bin (Div, e2, Num n2) ->
      let e1 = Bin (Mult, e1, Num n2) in
      let e2 = Bin (Mult, e2, Num n1) in
      let e1_e2 = n_opt (Bin (Plus, e1, e2)) in
      n_opt (n_div e1_e2 (Num (n1*n2)))*)
    (*
        e1         e1 + e2*e3
        --- + e3 = ---------
        e2           e2
      *)
(*    | Plus, Bin (Div, e1, Num n2), Num n3
    | Plus, Num n3, Bin (Div, e1, Num n2) ->
      n_opt (Bin (Div, Bin (Plus, e1, Num (n2*n3)), Num n2))*)
      (*
              n2    n1 * n2
        n1 * --- = --------
              e        e
      *)
(*    | Mult, Bin (Div, Num n2, e), Num n1
    | Mult, Num n1, Bin (Div, Num n2, e)
      ->
      n_opt (Bin (Div, Num (n1 * n2), e))*)
      (*
          n1          n1 / gcd n1 n2
          -------- = ------------------
          (n2 * e)   (n2 / gcd n1 n2) e

       *)
(*    | Div, Num n1, Bin (Mult, Num n2, e) when n2 <> 0 ->
      let g = gcd n1 n2 in
      Bin (Div, Num (n1/g), n_opt (Bin (Mult, Num (n2/g), e)))*)
(*    | Div, Num n1, Bin (Mult, e, Num n2) when n2 <> 0 ->
      let g = gcd n1 n2 in
      Bin (Div, Num (n1/g), n_opt (Bin (Mult, Num (n2/g), e)))
    | Div, Num n1, Num n2 when Common.modulo n1 n2 = 0 -> Num (n1 / n2)
    | Div, Num n1, Num n2 ->
      Bin (Div, Num (n1 / gcd n1 n2), Num (n2 / gcd n1 n2))
    | o, Num n1, Num n2 when o <> Div -> Num ((eval_nbin b) n1 n2)*)
    (* Propagate *)
    | _, _, _ -> Bin (b, a1, a2)

and b_opt (e : bexp) : bexp =
  match e with
  | ThreadEqual n ->
    begin match n_opt n with
    | Num _ -> Bool true
    | n -> ThreadEqual n
    end
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
    begin match b_opt b with
    | BNot b -> b
    | Bool b -> Bool (not b)
    | NRel (NEq, n1, n2) -> n_neq n1 n2
    | NRel (NNeq, n1, n2) -> n_eq n1 n2
    | NRel (NGt, n1, n2) -> n_le n1 n2
    | NRel (NLt, n1, n2) -> n_ge n1 n2
    | NRel (NLe, n1, n2) -> n_gt n1 n2
    | NRel (NGe, n1, n2) -> n_lt n1 n2
    | _ -> BNot b
    end

let r_opt (r:Range.t) : Range.t =
  {
    r with
    lower_bound = n_opt r.lower_bound;
    upper_bound = n_opt r.upper_bound;
  }

let a_opt (a:Access.t) : Access.t =
  {
    mode = a.mode;
    index = List.map n_opt a.index;
  }
