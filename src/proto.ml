type variable = string

let tid = "$tid"
let idx = "idx"

type nbin =
| Plus
| Minus
| Mult
| Div
| Mod

type nexp =
| Var of variable
| Num of int
| Bin of nbin * nexp * nexp

type nrel = NEq | NLe | NLt

type brel = BOr | BAnd

type bexp =
| Bool of bool
| NRel of nrel * nexp * nexp
| BRel of brel * bexp * bexp
| BNot of bexp
let n_lt n1 n2 =
  NRel (NLt, n1, n2)
let n_gt n1 n2 = n_lt n2 n1
let n_le n1 n2 = NRel (NLe, n1, n2)
let n_ge n1 n2 = n_le n2 n1

let n_eq n1 n2 =
  NRel (NEq, n1, n2)

let b_or b1 b2 =
  match b1, b2 with
  | Bool true, _ | _, Bool true -> Bool true
  | Bool false, b | b, Bool false -> b
  | _, _ -> BRel (BOr, b1, b2)

let b_and b1 b2 =
  match b1, b2 with
  | Bool true, b | b, Bool true -> b
  | Bool false, _ | _, Bool false -> Bool false
  | _, _ -> BRel (BAnd, b1, b2)

let b_not b = BNot b

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

type range = {range_var: variable; range_upper_bound: nexp}

type mode = R | W

type access = {access_index: nexp; access_cond: bexp; access_mode: mode}

type proto =
| Skip
| Sync
| Acc of variable * access
| Seq of proto * proto
| Loop of range * proto

(** A timed access is prefixed by the phase it was accessed *)
type 'a timed = {timed_phase: nexp; timed_data: 'a}

type access_t = access timed

type step_list = (string * access_t) list

type kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  kernel_locations: string list;
  (* The internal variables are used in the code of the kernel.  *)
  kernel_variables: string list;
  (* A precondition that restricts the values of internal variables. *)
  kernel_pre: bexp;
  (* The code of a kernel performs the actual memory accesses. *)
  kernel_code: proto;
}

