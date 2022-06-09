open Exp

type c_nexp =
  | Var of variable
  | Num of int
  | Bin of nbin * c_nexp * c_nexp
  | Assign of c_nexp * c_nexp
  | ArraySubscript of c_nexp * c_nexp
  | NIf of c_bexp * c_nexp * c_nexp


and c_bexp =
  | Bool of bool
  | NRel of nrel * c_nexp * c_nexp
  | BRel of brel * c_bexp * c_bexp
  | BNot of c_bexp

type c_range = {
  c_range_var: variable;
  c_range_lower_bound: c_nexp;
  c_range_upper_bound: c_nexp;
  c_range_step: step_expr;
}

type stmt =
| AExpr of c_nexp
| Decl of (variable * locality * c_nexp option) list
| Sync
| Block of (stmt list)
| Assert of c_bexp
| If of (c_bexp * stmt * stmt)
| For of (range * stmt)
| Loop of stmt


(*
  if ((x++) < 3)  {...}
  ---
  x = x + 1
  y = x
  if (y < 3) { .. }
  *)
