open Stage0

let (@) = Common.append_tr

let tid = "$tid"
let idx = "idx"

type nbin =
  | BitOr
  | BitXOr
  | BitAnd
  | LeftShift
  | RightShift
  | Plus
  | Minus
  | Mult
  | Div
  | Mod

type task =
  | Task1
  | Task2

let task_to_string (t:task) : string =
  match t with
  | Task1 -> "T1"
  | Task2 -> "T2"

let other_task = function
  | Task1 -> Task2
  | Task2 -> Task1

type nrel =
  | NEq
  | NNeq
  | NLt
  | NLe
  | NGt
  | NGe

type brel =
  | BOr
  | BAnd

type nexp =
  | Var of Variable.t
  | Num of int
  | Bin of nbin * nexp * nexp
  | Proj of task * Variable.t
  | NCall of string * nexp
  | NIf of bexp * nexp * nexp

and bexp =
  | Bool of bool
  | NRel of nrel * nexp * nexp
  | BRel of brel * bexp * bexp
  | BNot of bexp
  | Pred of string * nexp

let eval_nbin (o:nbin) : int -> int -> int =
  match o with
  | BitAnd -> (land)
  | BitXOr -> (lxor)
  | BitOr -> (lor)
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div -> (/)
  | Mod -> Common.modulo
  | LeftShift -> (lsl)
  | RightShift -> (lsr)

let eval_nrel o: int -> int -> bool =
  match o with
  | NEq -> (=)
  | NNeq -> (<>)
  | NLe -> (<=)
  | NGe -> (>=)
  | NLt -> (<)
  | NGt -> (>)

let eval_brel o : bool -> bool -> bool =
  match o with
  | BOr -> (||)
  | BAnd -> (&&)

let rec n_eval_res (n: nexp) : (int, string) Result.t =
  let (let*) = Result.bind in
  match n with
  | Var x -> Error ("n_eval: variable " ^ Variable.name x)
  | Num n -> Ok n
  | Bin (o, n1, n2) ->
    let* n1 = n_eval_res n1 in
    let* n2 = n_eval_res n2 in
    Ok (eval_nbin o n1 n2)
  | Proj _ -> Error "n_eval: proj"
  | NCall (x,_) -> Error ("n_eval: call " ^ x)
  | NIf (b, n1, n2) ->
    let* b = b_eval_res b in
    if b then n_eval_res n1 else n_eval_res n2

and b_eval_res (b: bexp) : (bool, string) Result.t =
  let (let*) = Result.bind in
  match b with
  | Bool b -> Ok b
  | NRel (o, n1, n2) ->
    let* n1 = n_eval_res n1 in
    let* n2 = n_eval_res n2 in
    Ok (eval_nrel o n1 n2)
  | BRel (o, b1, b2) ->
    let* b1 = b_eval_res b1 in
    let* b2 = b_eval_res b2 in
    Ok (eval_brel o b1 b2)
  | BNot b ->
    let* b = b_eval_res b in
    Ok (not b)
  | Pred (x, _) ->
    Error ("b_eval: pred " ^ x)

let n_eval_opt (n: nexp) : int option =
  n_eval_res n |> Result.to_option

let b_eval_opt (b: bexp) : bool option =
  b_eval_res b |> Result.to_option

let n_eval (n: nexp) : int =
  match n_eval_res n with
  | Ok n -> n
  | Error e -> failwith e

let b_eval (b: bexp) : bool =
  match b_eval_res b with
  | Ok b -> b
  | Error e -> failwith e

let n_rel o n1 n2 =
  match n1, n2 with
  | Num n1, Num n2 -> Bool (eval_nrel o n1 n2)
  | _, _ -> NRel (o, n1, n2)

let n_lt = n_rel NLt

let n_gt = n_rel NGt

let n_le = n_rel NLe

let n_ge = n_rel NGe

let n_eq = n_rel NEq

let n_neq = n_rel NNeq

let n_if b n1 n2 =
  match b with
  | Bool b -> if b then n1 else n2
  | _ -> NIf (b, n1, n2)

let n_plus n1 n2 =
  match n1, n2 with
  | Num 0, n | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 + n2)
  | Num n1, Bin (Plus, Num n2, e)
  | Bin (Plus, Num n1, e), Num n2 ->
    Bin (Plus, Num (n1 + n2), e)
  | _, Num _ -> Bin (Plus, n2, n1)
  | _, _ -> Bin (Plus, n1, n2)

let n_minus n1 n2 =
  match n1, n2 with
  | n, Num 0 -> n
  | Num n1, Num n2 -> Num (n1 - n2)
  | _, _ -> Bin (Minus, n1, n2)

let n_mult n1 n2 =
  match n1, n2 with
  | Num 1, n | n, Num 1 -> n
  | Num 0, _ | _, Num 0 -> Num 0
  | Num n1, Num n2 -> Num (n1 * n2)
  | Num n1, Bin (Mult, Num n2, e)
  | Bin (Mult, Num n1, e), Num n2 ->
    Bin (Mult, Num (n1 * n2), e)
  | _, Num _ -> Bin (Mult, n2, n1)
  | _, _ -> Bin (Mult, n1, n2)

let n_div n1 n2 =
  match n1, n2 with
  | Bin (Mult, e, Num n1), Num n2 when n1 > n2 ->
    Bin (Mult, e, Num (n1/n2))
  | Bin (Mult, Num n1, e), Num n2 when n1 > n2 ->
    Bin (Mult, e, Num (n1/n2))
  | _, Num 1 -> n1
  | Num 0, _ -> Num 0
  | _, Num 0 -> failwith ("Division by 0")
  | Num n1, Num n2 -> Num (n1 / n2)
  | _, _ -> Bin (Div, n1, n2)

let n_mod n1 n2 =
  match n1, n2 with
  | Num n1, Num n2 -> Num (Common.modulo n1 n2)
  | _, _ -> Bin (Mod, n1, n2)

let n_bin o n1 n2 =
  try
    match o, n1, n2 with
    | _, Num n1, Num n2 -> Num (eval_nbin o n1 n2)
    | Plus, _, _ -> n_plus n1 n2
    | Minus, _, _ -> n_minus n1 n2
    | Mult, _, _ -> n_mult n1 n2
    | Div, _, _ -> n_div n1 n2
    | Mod, _, _ -> n_mod n1 n2
    | _, _, _ -> Bin (o, n1, n2)
  with
    Division_by_zero -> Bin (o, n1, n2)

let b_rel o b1 b2 =
  match b1, b2 with
  | Bool b1, Bool b2 -> Bool (eval_brel o b1 b2)
  | _, _ -> BRel (o, b1, b2)

let b_or b1 b2 =
  match b1, b2 with
  | Bool true, _ | _, Bool true -> Bool true
  | Bool false, b | b, Bool false -> b
  | _, _ -> b_rel BOr b1 b2

let b_and b1 b2 =
  match b1, b2 with
  | Bool true, b | b, Bool true -> b
  | Bool false, _ | _, Bool false -> Bool false
  | _, _ -> b_rel BAnd b1 b2

let b_not b =
  match b with
  | BNot b -> b
  | NRel (NEq, n1, n2) -> NRel (NNeq, n1, n2)
  | NRel (NNeq, n1, n2) -> NRel (NEq, n1, n2)
  | Bool b -> Bool (not b)
  | _ -> BNot b

let b_impl b1 b2 =
  match b1 with
  | Bool true -> b2
  | Bool false -> Bool true
  | _ -> b_or (b_not b1) b2

let b_true = Bool true
let b_false = Bool false

let rec b_and_ex l =
  match l with
  | [] -> Bool true
  | [x] -> x
  | x::l -> b_and x (b_and_ex l)

let rec b_or_ex l =
  match l with
  | [] -> Bool true
  | [x] -> x
  | x::l -> b_or x (b_or_ex l)


type step_expr = Default of nexp | StepName of string

type direction =
  | Increase
  | Decrease

type range = {
  range_var: Variable.t;
  range_dir: direction;
  range_lower_bound: nexp;
  range_upper_bound: nexp;
  range_step: step_expr;
}


(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let mk_range (x:Variable.t) (ub:nexp) =
  {
    range_var = x;
    range_lower_bound = Num 0;
    range_upper_bound = ub;
    range_step = Default (Num 1);
    range_dir = Increase;
  }

let range_to_cond (r:range) : bexp =
  (match r.range_step with
  | Default (Num 1) -> []
  | Default n ->
    let lb = r.range_lower_bound in
    let ub = r.range_upper_bound in
    let x = Var r.range_var in
    [
      (* lb <= x < ub *)
      n_le lb x;
      n_lt x ub;
      (* (x + lb) % step  == 0 *)
      n_eq (n_mod (n_minus x lb) n) (Num 0);
      (* Ensure that the step is positive *)
      n_gt n (Num 0)
    ]
  | StepName name -> [Pred(name, Var r.range_var)]
  )
  @
  [
    n_le r.range_lower_bound (Var r.range_var);
    n_lt (Var r.range_var) r.range_upper_bound;
  ]
  |> b_and_ex

let range_has_next (r:range) : bexp =
  n_lt r.range_lower_bound r.range_upper_bound

let range_is_empty (r:range) : bexp =
  n_ge r.range_lower_bound r.range_upper_bound

let range_first (r:range) : bexp =
  n_eq (Var r.range_var) r.range_lower_bound

type mode =
  | R
  | W

(* An access pairs the index-expression with the access mode (R/W) *)
type access = {access_index: nexp list; access_mode: mode}

(* Access expression *)
type acc_expr = Variable.t * access

let distinct (idx:Variable.t list) : bexp =
  b_or_ex (List.map (fun x -> n_neq (Proj (Task1, x)) (Proj (Task2, x)) ) idx)


(* ------------------------------------------- *)

type hierarchy_t =
  | SharedMemory
  | GlobalMemory

type array_t = {
  array_hierarchy: hierarchy_t;
  array_size: int list; (* Empty means unknown *)
  array_type: string list; (* Empty means unknown *)
}

let mk_array (h:hierarchy_t) : array_t = {
  array_hierarchy = h;
  array_size = [];
  array_type = [];
}

let mk_array_map (h:hierarchy_t) (vs:Variable.t list) : array_t Variable.Map.t =
  vs
  |> List.map (fun x -> (x, mk_array h))
  |> Variable.MapUtil.from_list
