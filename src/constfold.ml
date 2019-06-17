open Proto

let rec norm (b:bexp) : bexp list =
  match b with
  | Pred _
  | BNot (Pred _)
  | BRel (BOr, _, _)
  | BNot (NRel (NEq, _, _))
  | Bool _
  | NRel _ -> [b]
  | BRel (BAnd, b1, b2) -> List.append (norm b1) (norm b2)
  | BNot (Bool b) -> [Bool (not b)]
  | BNot (BRel (BAnd, b1, b2)) -> norm (b_or (b_not b1) (b_not b2))
  | BNot (BRel (BOr, b1, b2)) -> norm (b_and (b_not b1) (b_not b2))
  | BNot (NRel (NLt, n1, n2)) -> norm (NRel (NLe, n1, n2))
  | BNot (NRel (NLe, n1, n2)) -> norm (NRel (NLt, n1, n2))
  | BNot (BNot b) -> norm b

let rec n_opt (a : nexp) : nexp =
  match a with
  | Var _ -> a
  | Num n -> if n < 0 then raise (Failure "Negative number") else a
  | Proj (t, n) -> Proj(t, n_opt n)
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
    (* Compute *)
    | _, Num n1, Num n2 -> Num ((eval_nbin b) n1 n2)
    (* Propagate *)
    | _, _, _ -> Bin (b, a1, a2)

let bexp_to_bool b =
  match b with
  | Bool b -> Some b
  | _ -> None

let rec b_opt (e : bexp) : bexp =
match e with
| Pred _
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
    range_var = r.range_var;
    range_upper_bound = n_opt r.range_upper_bound;
  }

let a_opt a =
  {
    access_mode = a.access_mode;
    access_cond = b_opt a.access_cond;
    access_index = List.map n_opt a.access_index;
  }

let ta_opt ({timed_phase=p; timed_data=d}:access timed) =
  {timed_phase=n_opt p; timed_data=a_opt d}

let stream_opt (l:(variable * access timed) list) : (variable *access timed) list =
  let keep_acc (_,a) =
    match a with
    | {
        timed_phase = _;
        timed_data = {
          access_index = _;
          access_mode = _;
          access_cond = c;
        }
      } ->
      begin
        match c with
        | Bool false -> false
        | _ -> true
      end
  in
    List.map (fun (x, a) -> (x, ta_opt a)) l |> List.filter keep_acc

