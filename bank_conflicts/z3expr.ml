open Protocols

open Exp
open Z3
open Z3.Expr

exception Not_implemented of string

let nbin_to_expr (op:nbin) : context -> expr -> expr -> expr = match op with
  | BitOr
  | BitXOr
  | BitAnd
  | LeftShift
  | RightShift ->
    let op : string = Exp.nbin_to_string op in
    raise (Not_implemented ("nbin_to_expr not implemented for " ^ op))
  | Plus  -> fun ctx n1 n2 -> Arithmetic.mk_add ctx [n1; n2]
  | Minus -> fun ctx n1 n2 -> Arithmetic.mk_sub ctx [n1; n2]
  | Mult  -> fun ctx n1 n2 -> Arithmetic.mk_mul ctx [n1; n2]
  | Div   -> Arithmetic.mk_div
  | Mod   -> Arithmetic.Integer.mk_mod

let nrel_to_expr : nrel -> context -> expr -> expr -> expr = function
  | NEq  -> Boolean.mk_eq
  | NNeq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
  | NLe  -> Arithmetic.mk_le
  | NGe  -> Arithmetic.mk_ge
  | NLt  -> Arithmetic.mk_lt
  | NGt  -> Arithmetic.mk_gt

let brel_to_expr : brel -> context -> expr -> expr -> expr = function
  BOr  -> fun ctx b1 b2 -> Boolean.mk_or  ctx [b1; b2]
| BAnd -> fun ctx b1 b2 -> Boolean.mk_and ctx [b1; b2]

let rec n_to_expr (ctx:context) (n:nexp) : expr = match n with
| Var x -> Variable.name x |> Arithmetic.Integer.mk_const_s ctx
| Proj _
| NCall _ ->
    let n : string = Exp.n_to_string n in
    raise (Not_implemented ("n_to_expr not implemented for " ^ n))
| Num (n:int) -> Arithmetic.Integer.mk_numeral_i ctx n
| Bin (op, n1, n2) ->
    (nbin_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
| NIf (b, n1, n2) -> Boolean.mk_ite ctx
    (b_to_expr ctx b) (n_to_expr ctx n1) (n_to_expr ctx n2)

and b_to_expr (ctx:context) (b:bexp) : expr = match b with
  Bool (b:bool) -> Boolean.mk_val ctx b
| NRel (op, n1, n2) ->
    (nrel_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
| BRel (op, b1, b2) ->
    (brel_to_expr op) ctx (b_to_expr ctx b1) (b_to_expr ctx b2)
| BNot (b:bexp) -> Boolean.mk_not ctx (b_to_expr ctx b)
| Pred _ -> raise (Not_implemented "b_to_expr not implemented for Pred")
