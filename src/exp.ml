type variable = {
  var_loc: Sourceloc.location;
  var_name: string;
}

let var_make (name:string) = {
  var_loc = Sourceloc.loc_empty;
  var_name = name;
}

let var_of_loc name pair =
  {
    var_loc = Sourceloc.of_lex_position_pair pair;
    var_name = name;
  }

let var_equal (x:variable) (y:variable) = String.equal x.var_name y.var_name

module VarOT = struct
  type t = variable
  let compare = fun x y -> Stdlib.compare x.var_name y.var_name
end

module VarSet = Set.Make(VarOT)

let tid = "$tid"
let idx = "idx"

type nbin =
| Plus
| Minus
| Mult
| Div
| Mod

type task = Task1 | Task2
let task_to_string (t:task) : string =
  match t with
  | Task1 -> "T1"
  | Task2 -> "T2"

let other_task = function
  | Task1 -> Task2
  | Task2 -> Task1

type nexp =
| Var of variable
| Num of int
| Bin of nbin * nexp * nexp
| Proj of task * variable

type nrel = NEq | NNeq | NLe | NLt | NGt | NGe

type brel = BOr | BAnd

type bexp =
| Bool of bool
| NRel of nrel * nexp * nexp
| BRel of brel * bexp * bexp
| BNot of bexp
| Pred of string * nexp

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
  | NNeq -> (<>)
  | NLe -> (<=)
  | NGe -> (>=)
  | NLt -> (<)
  | NGt -> (>)

let eval_brel o : bool -> bool -> bool =
  match o with
  | BOr -> (||)
  | BAnd -> (&&)

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

let n_bin o n1 n2 =
  match n1, n2 with
  | Num n1, Num n2 -> Num (eval_nbin o n1 n2)
  | _, _ -> Bin (o, n1, n2)

let n_plus n1 n2 =
  match n1, n2 with
  | Num 0, n | n, Num 0 -> n
  | _, _ -> n_bin Plus n1 n2

let n_minus n1 n2 =
  match n1, n2 with
  | Num 0, n | n, Num 0 -> n
  | _, _ -> n_bin Minus n1 n2

let n_mult = n_bin Mult

let n_div n1 n2 =
  match n1, n2 with
  | _, Num 1 -> n1
  | Num 0, _ -> Num 0
  | _, Num 0 -> raise (Failure "Division by 0")
  | _, _ -> n_bin Div n1 n2

let n_mod n1 n2 =
  match n1, n2 with
  | _, Num 1 -> Num 0
  | _, Num 0 -> raise (Failure "Modulo by 0")
  | _, _ -> n_bin Mod n1 n2

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

let n_neq b1 b2 = b_not (n_eq b1 b2)

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

type range = {
  range_var: variable;
  range_lower_bound: nexp;
  range_upper_bound: nexp
}

type mode = R | W

(* An access pairs the index-expression with the access mode (R/W) *)
type access = {access_index: nexp list; access_mode: mode}

let distinct (idx:variable list) : bexp =
  b_or_ex (List.map (fun x -> n_neq (Proj (Task1, x)) (Proj (Task2, x)) ) idx)
