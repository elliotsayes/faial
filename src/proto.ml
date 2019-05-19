type variable = string

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
| BNot of bexp * bexp

type set = {
  (* elem | idx \in [0..upper_bound) if cond *)
  set_elem: nexp;
  set_upper_bound: nexp;
  set_cond: bexp;
}

type range = {range_var: variable; range_upper_bound: nexp}

type mode = R | W

type access = {access_set: set; access_mode: mode}

type proto =
| Skip
| Sync
| Acc of variable * access
| Seq of proto * proto
| Loop of range * proto
