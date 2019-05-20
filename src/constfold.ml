open Proto

let eval_nbin (o:nbin) : int -> int -> int =
  match o with
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div -> (/)
  | Mod -> (mod)


let eval_nrel o: int -> int -> bool =
  match o with
  | NEq -> (=)
  | NLe -> (<=)
  | NLt -> (<)


let eval_brel o : bool -> bool -> bool =
  match o with
  | BOr -> (||)
  | BAnd -> (&&)


let rec n_opt (a : nexp) : nexp =
  match a with
  | Var _ -> a
  | Num n -> if n < 0 then raise (Failure "Negative number") else a
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
| Bool _ -> e
| BRel (b, b1, b2) ->
  begin
    let b1 = b_opt b1 in
    let b2 = b_opt b2 in
    match bexp_to_bool b1, bexp_to_bool b2 with
    | Some b1, Some b2 -> Bool ((eval_brel b) b1 b2)
    | _, _ -> BRel (b, b1, b2)
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

let s_opt ({set_elem=e;set_upper_bound=ub;set_cond=b}:set) : set =
  {
    set_elem = n_opt e;
    set_upper_bound = n_opt ub;
    set_cond = b_opt b;
  }

let a_opt ({access_set=s; access_mode=m}) =
  {access_set=s_opt s; access_mode=m}

let ta_opt ({timed_phase=p; timed_data=d}:access timed) =
  {timed_phase=n_opt p; timed_data=a_opt d}

let ota_opt ({owned_tid=t; owned_data=d}:access timed owned) =
  {owned_tid=t; owned_data=ta_opt d}

let stream_opt (l:(string * access timed owned) list) : (string *access timed owned) list =
  let keep_acc (_,a) =
    match a with
    | {owned_tid = _;
        owned_data = {
          timed_phase = _;
          timed_data = {
            access_set = {
              set_elem = _; set_upper_bound = ub; set_cond = bc
            };
            access_mode = _
          }
        }
      } ->
      begin
        match ub,bc with
        | Num 0, _ | _, Bool false -> false
        | _, _ -> true
      end
  in
    List.map (fun (x, a) -> (x, ota_opt a)) l |> List.filter keep_acc

