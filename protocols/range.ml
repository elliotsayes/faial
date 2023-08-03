open Stage0
open Exp

module Step = struct
  type t = Plus of nexp | Mult of nexp

  let plus (e:nexp) : t = Plus e

  let mult (e:nexp) : t = Mult e

  let to_string : t -> string =
    function
    | Plus x -> "+= " ^ n_to_string x
    | Mult x -> "*= " ^ n_to_string x

  let inc : t -> nexp -> nexp =
    function
    | Plus n -> n_plus n
    | Mult n -> n_mult n

  let dec : t -> nexp -> nexp =
    function
    | Mult n -> fun m -> n_div m n
    | Plus n -> fun m -> n_minus m n

  let stride : t -> nexp =
    function
    | Plus e -> e
    | Mult e -> e

  let is_valid : t -> bexp =
    function
    | Plus e -> n_gt e (Num 0)
    | Mult e -> n_gt e (Num 1)

  let map (f:nexp -> nexp) : t -> t =
    function
    | Plus n -> Plus (f n)
    | Mult n -> Mult (f n)

  let eval_res : t -> (int -> int, string) Result.t =
    let ( let* ) = Result.bind in
    function
    | Plus n ->
      let* x = n_eval_res n in
      Ok (fun y -> x + y)
    | Mult n ->
      let* x = n_eval_res n in
      Ok (fun y -> x * y)

end

type direction =
  | Increase
  | Decrease

type t = {
  var: Variable.t;
  dir: direction;
  lower_bound: nexp;
  upper_bound: nexp;
  step: Step.t;
}


let to_string (r : t) : string =
  let x = Variable.name r.var in
  let lb = n_to_string r.lower_bound in
  let ub = n_to_string r.upper_bound in
  let s = match r.step with
  | Plus (Num 1) -> ""
  | _ -> "; " ^ Variable.name r.var ^ " " ^ Step.to_string r.step
  in
  let d = match r.dir with
  | Increase -> "↑"
  | Decrease -> "↓"
  in
  x ^ " in " ^ lb ^ " .. " ^ ub ^ s ^ " " ^ d

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let map (f: nexp -> nexp) (r: t) : t =
  { r with
    lower_bound = f r.lower_bound;
    upper_bound = f r.upper_bound;
    step = Step.map f r.step;
  }


let make
  ?(lower_bound=Num 0)
  ?(step:Step.t=Plus (Num 1))
  ?(dir = Increase)
  (x:Variable.t)
  (ub:nexp) =
  {
    var = x;
    lower_bound = lower_bound;
    upper_bound = ub;
    step = step;
    dir = dir;
  }

let eq_nums x l : bexp =
  List.map (fun i -> n_eq x (Num i)) l
  |> b_or_ex

let pow ~base (n:nexp) : bexp =
  let ub = 0xFFFFFFFF in
  (* Generate a list of powers *)
  let rec pows (n:int) : int list =
    let x = Common.pow ~base n in
    if x > ub then []
    else if x == ub then [x]
    else x :: pows (n + 1)
  in
  pows 0 |> eq_nums n

let to_cond (r:t) : bexp =
  let x = Var r.var in
  let lb = r.lower_bound in
  let ub = r.upper_bound in
  (match r.step with
  | Plus (Num 1) -> []
  | Plus n ->
    [
      (* (x + lb) % step  == 0 *)
      n_eq (n_mod (n_minus x lb) n) (Num 0);
      (* Ensure that the step is positive *)
      (* n > 0 *)
      n_gt n (Num 0)
    ]
  | Mult (Num base) -> [
      pow ~base x;
      (* base > 1 *)
      n_gt (Num base) (Num 1)
    ]
  | Mult e ->
    prerr_endline ("range_to_cond: unsupported range: " ^ Exp.n_to_string e);
    [
      (* Ensure that the step is positive *)
      n_gt e (Num 1)
    ]
  )
  @
  [
    (* lb <= x < ub *)
    n_le lb x; n_lt x ub;
  ]
  |> b_and_ex

let prev (r:t) : nexp =
  match r.dir with
  | Increase -> Step.dec r.step (Var r.var)
  | Decrease -> Step.inc r.step (Var r.var)

let has_next (r:t) : bexp =
  n_lt r.lower_bound r.upper_bound

let is_empty (r:t) : bexp =
  n_ge r.lower_bound r.upper_bound

let is_first (r:t) : bexp =
  n_eq (Var r.var) r.lower_bound

let first (r:t) : nexp =
  match r.dir with
  | Increase -> r.lower_bound
  | Decrease -> r.upper_bound

(*
  3 ... 12  += 3

  3 5 7 9 11

  12 - 3

  12 - (3 % step ) = 11

  4 ... 15 += 3

  4 7 10 13

  up - (step - lb % step) = 13 - (3 - 4 % 3) = 13 - (3 - 1) = 13 - 2 = 11

 *)

let last_plus ~lower_bound ~upper_bound (step:nexp) : nexp =
  n_minus upper_bound
    (n_minus step (n_mod lower_bound step))

(*
  2 ... 13  -= 3

  13 10 7 4

    lb + step - ub % step
  = 2 + 3 - 13 % 3
  = 2 + 3 - 1
  = 2 + 2
  = 4

 *)

let last_minus ~lower_bound ~upper_bound (step:nexp) : nexp =
  n_plus
    lower_bound
    (n_minus step (n_mod upper_bound step))

let highest_power ~base : nexp -> nexp =
  let rec gen (n:int) (x:nexp) : nexp =
    if n <= 0 then (Num 1)
    else
      let p = Num (Common.pow ~base n) in
      NIf (n_gt x p, p, gen (n - 1) x)
  in
  let trunc_fun (n:nexp) : nexp =
    gen base n
  in
  function
  | Num n -> Num (Common.highest_power ~base n)
  | e -> trunc_fun e

(*

  4 ... 15 *= 3 = [4, 12]
    log_step(ub/lb)
  = log_step(15/4)
  = 1

    lb * step ^ log_step(ub/lb) =
  = 4 * 3 ^ 1
  = 4 * 3
  = 12

  3 ... 76 *= 5 -> [3, 15, 75]

  log_step(ub/lb) = 2

  lower_bound * step ^ log_step(ub/lb) = 3 * 5^2 = 75

 *)

let last_mult ~lower_bound ~upper_bound (step:int) : nexp =
  if step = 2 then
    Bin (
      Mult,
      lower_bound,
      Bin (LeftShift, Num 2, Bin (Div, upper_bound, lower_bound))
    )
  else
    failwith ("last_mult: invalid base: " ^ string_of_int step)

(*

  4 ... 15 /= 3 = [15, 5]

    log_step(ub/lb)
  = log_step(15/4)
  = 1

    ub / step ^ log_step(ub/lb) =
  = 15 / 3 ^ 1
  = 15 / 3
  = 5

 *)
let last_div ~lower_bound ~upper_bound (step:int) : nexp =
  if step = 2 then
    match lower_bound with
    | Num 1 -> Num 1
    | _ ->
      Bin (
        Div,
        upper_bound,
        (* 2 << (ub/lb) *)
        Bin (LeftShift, Num 2, Bin (Div, lower_bound, upper_bound))
      )
  else
    failwith ("last_mult: invalid base: " ^ string_of_int step)

let last (r:t) : nexp option =
  match r.dir, r.step with
  | Increase, Plus s ->
    Some (last_plus ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | Decrease, Plus s ->
    Some (last_minus ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | Increase, Mult (Num s) when s = 2 ->
    Some (last_mult ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | Decrease, Mult (Num s) when s = 2 ->
    Some (last_div ~lower_bound:r.lower_bound ~upper_bound:r.upper_bound s)
  | _ -> None

(* Returns the last element of a bound *)

let lossy_last (r:t) : nexp =
  match last r with
  | Some e -> e
  | None ->
    prerr_endline ("WARNING: lossy_last: unsupported base: " ^ to_string r);
    last_plus
      ~lower_bound:r.lower_bound
      ~upper_bound:r.upper_bound
      (Num 1)

let stride (r:t) : nexp =
  Step.stride r.step

(* The first element in a while loop *)
let while_init (r:t) : nexp =
  match r.dir with
  | Increase -> r.lower_bound
  | Decrease -> r.upper_bound

(* The condition in a while loop *)
let while_cond (r:t) : bexp =
  match r.dir with
  | Increase -> n_lt (Var r.var) r.upper_bound
  | Decrease -> n_ge (Var r.var) r.lower_bound

(* An increment of a while loop *)
let while_inc (r:t) : nexp =
  let x = Var r.var in
  let o, e1, e2 = match r.dir, r.step with
    | Increase, Step.Plus e -> Plus, x, e
    | Decrease, Step.Plus e -> Minus, x, e
    | Increase, Step.Mult e -> Mult, x, e
    | Decrease, Step.Mult e -> Div, x, e
  in
  Bin (o, e1, e2)

let next (r:t) : t =
  match r.dir with
  | Increase ->
    { r with lower_bound = Step.inc r.step r.lower_bound }
  | Decrease ->
    { r with upper_bound = Step.dec r.step r.upper_bound }

let is_valid (r:t) : bexp =
  b_and (Step.is_valid r.step) (
  match r.step with
  | Mult _ -> n_neq r.lower_bound (Num 0)
  | Plus _ -> Bool true
  )

let eval_res (r:t) : (int list, string) Result.t =
  let ( let* ) = Result.bind in
  let* lb = n_eval_res r.lower_bound in
  let* ub = n_eval_res r.upper_bound in
  let* b = b_eval_res (is_valid r) in
  let* inc = Step.eval_res r.step in
  if b then
    let rec iter lb =
      if lb < ub
      then lb :: iter (inc lb)
      else []
    in
    Ok (iter lb)
  else
    Error ("Invalid range: " ^ to_string r)

let eval_opt (r:t) : int list option =
  eval_res r |> Result.to_option
