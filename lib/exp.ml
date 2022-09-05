let (@) = Common.append_tr

type variable =
  | LocVariable of Sourceloc.location * string
  | Variable of string

let var_make (name:string) : variable = Variable name

let var_set_name (v:variable) (name:string) =
  match v with
  | LocVariable(l, _) -> LocVariable(l, name)
  | Variable _ -> Variable name

let var_of_loc name pair =
  LocVariable (Sourceloc.of_lex_position_pair pair, name)

let var_name (x:variable) : string =
  match x with
  | LocVariable (_, x) -> x
  | Variable x -> x

let var_loc_opt (x:variable) : Sourceloc.location option =
  match x with
  | LocVariable (l, _) -> Some l
  | Variable _ -> None

let var_set_loc (l:Sourceloc.location) (x:variable) : variable =
  LocVariable (l, var_name x)

let var_loc (x:variable) : Sourceloc.location =
  var_loc_opt x |> Option.value ~default:Sourceloc.loc_empty

let var_equal (x:variable) (y:variable) =
  String.equal (var_name x) (var_name y)

let var_repr (x:variable) : string =
  match x with
  | Variable x -> "Variable{name=\""^ x ^"\"}"
  | LocVariable (l, x) ->
    "LocVariable{name=\"" ^ x ^ "\", loc="^ Sourceloc.location_repr l ^ "}"

module VarOT = struct
  type t = variable
  let compare = fun x y -> Stdlib.compare (var_name x) (var_name y)
end

module VarSet = Set.Make(VarOT)
module VarMap = Map.Make(VarOT)
module VarMapUtil = Common.MapUtil(VarMap)

let var_set_to_map (s:VarSet.t) (f:variable -> 'a option) : 'a VarMap.t =
  VarSet.fold (fun k m ->
    match f k with
    | Some v -> VarMap.add k v m
    | None -> m
  ) s VarMap.empty

let var_map_to_set (m:'a VarMap.t) : VarSet.t =
  VarMap.bindings m |> List.map fst |> VarSet.of_list

let list_to_var_map (l:(variable * 'a) list) : 'a VarMap.t =
  List.fold_left (fun m (k,v) ->
    VarMap.add k v m
  ) VarMap.empty l

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
  | Var of variable
  | Num of int
  | Bin of nbin * nexp * nexp
  | Proj of task * variable
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

let rec n_eval (n: nexp) : int =
  match n with
  | Var x -> failwith ("n_eval: variable " ^ (var_name x))
  | Num n -> n
  | Bin (o, n1, n2) -> eval_nbin o (n_eval n1) (n_eval n2)
  | Proj _ -> failwith ("n_eval: proj")
  | NCall (x,_) -> failwith ("n_eval: call " ^ x)
  | NIf (b, n1, n2) ->
    if (b_eval b) then (n_eval n1) else (n_eval n2)
and b_eval (b: bexp) : bool =
  match b with
  | Bool b -> b
  | NRel (o, n1, n2) ->
    eval_nrel o (n_eval n1) (n_eval n2)
  | BRel (o, b1, b2) ->
    eval_brel o (b_eval b1) (b_eval b2)
  | BNot b ->
    not (b_eval b)
  | Pred (x, _) ->
    failwith ("b_eval: pred " ^ x)

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
  range_var: variable;
  range_dir: direction;
  range_lower_bound: nexp;
  range_upper_bound: nexp;
  range_step: step_expr;
}


(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let mk_range (x:variable) (ub:nexp) =
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
type acc_expr = variable * access

let distinct (idx:variable list) : bexp =
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

let mk_array_map (h:hierarchy_t) (vs:variable list) : array_t VarMap.t =
  vs
  |> List.map (fun x -> (x, mk_array h))
  |> list_to_var_map
