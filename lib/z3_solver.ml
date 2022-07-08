open Exp
module Solver = Z3.Solver
module Expr = Z3.Expr
module Boolean = Z3.Boolean
module Arithmetic = Z3.Arithmetic
module Integer = Z3.Arithmetic.Integer
module Model = Z3.Model

exception Not_implemented of string

let nbin_to_expr (op:nbin) : Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = match op with
  BitOr
| BitXOr
| BitAnd
| LeftShift
| RightShift ->
  let op : string = Serialize.nbin_to_string op in
  raise (Not_implemented ("nbin_to_expr not implemented for " ^ op))
| Plus  -> fun ctx n1 n2 -> Arithmetic.mk_add ctx [n1; n2]
| Minus -> fun ctx n1 n2 -> Arithmetic.mk_sub ctx [n1; n2]
| Mult  -> fun ctx n1 n2 -> Arithmetic.mk_mul ctx [n1; n2]
| Div   -> Arithmetic.mk_div
| Mod   -> Arithmetic.Integer.mk_mod

let nrel_to_expr : nrel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
  NEq  -> Boolean.mk_eq
| NNeq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
| NLe  -> Arithmetic.mk_le
| NGe  -> Arithmetic.mk_ge
| NLt  -> Arithmetic.mk_lt
| NGt  -> Arithmetic.mk_gt

let brel_to_expr : brel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
  BOr  -> fun ctx b1 b2 -> Boolean.mk_or  ctx [b1; b2]
| BAnd -> fun ctx b1 b2 -> Boolean.mk_and ctx [b1; b2]

let rec n_to_expr (ctx:Z3.context) (n:nexp) : Expr.expr = match n with
| Var x -> var_name x |> Integer.mk_const_s ctx
| Proj _
| NCall _ ->
    let n : string = Serialize.PPrint.n_to_s n in
    raise (Not_implemented ("n_to_expr not implemented for " ^ n))
| Num (n:int) -> Arithmetic.Integer.mk_numeral_i ctx n
| Bin (op, n1, n2) ->
    (nbin_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
| NIf (b, n1, n2) -> Boolean.mk_ite ctx
    (b_to_expr ctx b) (n_to_expr ctx n1) (n_to_expr ctx n2)

and b_to_expr (ctx:Z3.context) (b:bexp) : Expr.expr = match b with
  Bool (b:bool) -> Boolean.mk_val ctx b
| NRel (op, n1, n2) ->
    (nrel_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
| BRel (op, b1, b2) ->
    (brel_to_expr op) ctx (b_to_expr ctx b1) (b_to_expr ctx b2)
| BNot (b:bexp) -> Boolean.mk_not ctx (b_to_expr ctx b)
| Pred _ -> raise (Not_implemented "b_to_expr not implemented for Pred")


let add (s:Solver.solver) (ctx:Z3.context) (p:Symbexp.proof) : unit =
	prerr_endline ("~~~~~~~~~~~~~~~~~ Array " ^ p.proof_name ^ " ~~~~~~~~~~~~~~~~~~~");
	let mk_var (name:string) : Expr.expr =
		let x = Integer.mk_const_s ctx name in
		Arithmetic.mk_ge ctx x (Integer.mk_numeral_i ctx 0)
	in
	List.map mk_var p.proof_decls |> Solver.add s;
	let assign x n =
		Boolean.mk_eq ctx (Integer.mk_const_s ctx x) (Integer.mk_numeral_i ctx n)
	in
	[assign "blockDim.y" 1; assign "blockDim.z" 1; b_to_expr ctx p.proof_goal] |> Solver.add s
	

let solve ((cache, ps):(Symbexp.LocationCache.t * Symbexp.proof Streamutil.stream)) : unit =
	let ctx = Z3.mk_context [
		("model", "true");
		("proof", "false");
		("timeout", "500");
	] in
	let s = Solver.mk_simple_solver ctx in
	Streamutil.iter (fun p ->
		add s ctx p;
		prerr_endline (Solver.to_string s);
		prerr_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
		match Solver.check s [] with
		| UNSATISFIABLE -> print_endline "DRF"
		| SATISFIABLE ->
			(match Solver.get_model s with
			| Some m ->
				print_endline "RACY";
				prerr_endline (Model.to_string m)
			| None -> print_endline "INVALID")
		| UNKNOWN -> print_endline "UNKNOWN"
	) ps
