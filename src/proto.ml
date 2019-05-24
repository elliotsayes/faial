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

type range = {range_var: variable; range_upper_bound: nexp}

type set = {
  (* elem | idx \in [0..upper_bound) if cond *)
  set_elem: nexp;
  set_range: range option;
  set_cond: bexp;
}

type mode = R | W

type access = {access_set: set; access_mode: mode}

type proto =
| Skip
| Sync
| Acc of variable * access
| Seq of proto * proto
| Loop of range * proto

(** A timed access is prefixed by the phase it was accessed *)
type 'a timed = {timed_phase: nexp; timed_data: 'a}
type 'a owned = {owned_tid: string; owned_data: 'a}

type access_t = access timed owned
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

