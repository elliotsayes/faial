open Exp
module Solver = Z3.Solver
module Expr = Z3.Expr
module Boolean = Z3.Boolean
module Arithmetic = Z3.Arithmetic
module Integer = Z3.Arithmetic.Integer
module Model = Z3.Model
module Symbol = Z3.Symbol
module FuncDecl = Z3.FuncDecl
module BitVector = Z3.BitVector

module StringMap = Common.StringMap
type json = Yojson.Basic.t

exception Not_implemented of string

module type GEN = sig
	val n_to_expr: Z3.context -> nexp -> Expr.expr
	val b_to_expr: Z3.context -> bexp -> Expr.expr
end

module IntGen : GEN = struct 

	let nbin_to_expr (op:nbin) : Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = match op with
		| BitAnd
		| BitOr
		| BitXOr
		| LeftShift
		| RightShift ->
		  fun ctx n1 n2 ->
		  let op : string = Serialize.nbin_to_string op in
      prerr_endline ("WARNING: operator '" ^ op ^ "' unsupported; converting to '+'. Use bit-vector integers instead");
		  Arithmetic.mk_add ctx [n1; n2]
		| Plus  -> fun ctx n1 n2 -> Arithmetic.mk_add ctx [n1; n2]
		| Minus -> fun ctx n1 n2 -> Arithmetic.mk_sub ctx [n1; n2]
		| Mult  -> fun ctx n1 n2 -> Arithmetic.mk_mul ctx [n1; n2]
		| Div   -> Arithmetic.mk_div
		| Mod   -> Arithmetic.Integer.mk_mod

	let nrel_to_expr : nrel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
		| NEq  -> Boolean.mk_eq
		| NNeq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
		| NLe  -> Arithmetic.mk_le
		| NGe  -> Arithmetic.mk_ge
		| NLt  -> Arithmetic.mk_lt
		| NGt  -> Arithmetic.mk_gt

	let brel_to_expr : brel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
		| BOr  -> fun ctx b1 b2 -> Boolean.mk_or  ctx [b1; b2]
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
		| Bool (b:bool) -> Boolean.mk_val ctx b
		| NRel (op, n1, n2) ->
		    (nrel_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
		| BRel (op, b1, b2) ->
		    (brel_to_expr op) ctx (b_to_expr ctx b1) (b_to_expr ctx b2)
		| BNot (b:bexp) -> Boolean.mk_not ctx (b_to_expr ctx b)
		| Pred _ -> raise (Not_implemented "b_to_expr not implemented for Pred")
end

let word_size = 64

module BVGen : GEN = struct
	let nbin_to_expr (op:nbin) : Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = match op with
		| BitAnd -> BitVector.mk_and
		| BitOr -> BitVector.mk_or
		| BitXOr -> BitVector.mk_xor
		| LeftShift -> BitVector.mk_shl
		| RightShift -> BitVector.mk_ashr
		| Plus  -> BitVector.mk_add
		| Minus -> BitVector.mk_sub
		| Mult  -> BitVector.mk_mul
		| Div   -> BitVector.mk_udiv
		| Mod   -> BitVector.mk_smod

	let nrel_to_expr : nrel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
		| NEq  -> Boolean.mk_eq
		| NNeq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
		| NLe  -> BitVector.mk_ule
		| NGe  -> BitVector.mk_uge
		| NLt  -> BitVector.mk_ult
		| NGt  -> BitVector.mk_ugt

	let brel_to_expr : brel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
		| BOr  -> fun ctx b1 b2 -> Boolean.mk_or  ctx [b1; b2]
		| BAnd -> fun ctx b1 b2 -> Boolean.mk_and ctx [b1; b2]

	let rec n_to_expr (ctx:Z3.context) (n:nexp) : Expr.expr = match n with
		| Var x -> BitVector.mk_const_s ctx (var_name x) word_size
		| Proj _
		| NCall _ ->
		    let n : string = Serialize.PPrint.n_to_s n in
		    raise (Not_implemented ("n_to_expr not implemented for " ^ n))
		| Num (n:int) -> BitVector.mk_numeral ctx (string_of_int n) word_size
		| Bin (op, n1, n2) ->
		    (nbin_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
		| NIf (b, n1, n2) -> Boolean.mk_ite ctx
		    (b_to_expr ctx b) (n_to_expr ctx n1) (n_to_expr ctx n2)

	and b_to_expr (ctx:Z3.context) (b:bexp) : Expr.expr = match b with
		| Bool (b:bool) -> Boolean.mk_val ctx b
		| NRel (op, n1, n2) ->
		    (nrel_to_expr op) ctx (n_to_expr ctx n1) (n_to_expr ctx n2)
		| BRel (op, b1, b2) ->
		    (brel_to_expr op) ctx (b_to_expr ctx b1) (b_to_expr ctx b2)
		| BNot (b:bexp) -> Boolean.mk_not ctx (b_to_expr ctx b)
		| Pred _ -> raise (Not_implemented "b_to_expr not implemented for Pred")
end

let b_to_expr = IntGen.b_to_expr

let add (s:Solver.solver) (ctx:Z3.context) (p:Symbexp.proof) : unit =
	
	let mk_var (name:string) : Expr.expr =
		n_ge (Var (Exp.var_make name)) (Num 0)
		|> b_to_expr ctx
	in
	List.map mk_var p.proof_decls |> Solver.add s;
	let assign x n =
		n_eq (Var (Exp.var_make x)) (Num n)
		|> b_to_expr ctx
	in
	[
		assign "blockDim.y" 1;
		assign "blockDim.z" 1;
		b_to_expr ctx p.proof_goal
	]
	|> Solver.add s
	
module Environ = struct
	type t = (string * string) list

	let to_json (env:t) : json =
		let open Yojson.Basic in
		`Assoc (
			List.map (fun (k, v) -> (k, `String v)) env
		)

	let to_string (env:t) : string =
		let open Yojson.Basic in
		to_json env |> pretty_to_string

	let get: string -> t -> string option = List.assoc_opt

	let parse (m:Model.model) : t =
		Model.get_const_decls m
		|> List.map (fun d ->
			let key: string = FuncDecl.get_name d |> Symbol.get_string in
			let e : string = FuncDecl.apply d []
				|> (fun e -> Model.eval m e true)
				|> Option.map Expr.to_string
				|> Ojson.unwrap_or "?"
			in
			(key, e)
		)

end

module Vec3 = struct
	type t = {x : string; y: string; z: string;}
	let mk ~x:x ~y:y ~z:z : t = {x=x; y=y; z=z}

	let to_json (v:t) : json =
		let open Yojson.Basic in
		`Assoc [
			"x", `String v.x;
			"y", `String v.y;
			"z", `String v.z;
		]

	let to_string (v:t) =
		let open Yojson.Basic in
		to_json v |> pretty_to_string

	let parse (kvs:Environ.t) : (t * t) =
		let parse_vec (suffix:string) : t =
			let parse (x:string) : string =
				Environ.get ("threadIdx." ^ x ^ "$T" ^ suffix) kvs |> Ojson.unwrap_or "0"
			in
			{x=parse "x"; y=parse "y"; z=parse "z"}
		in
		(parse_vec "1", parse_vec "2")

end

module Task = struct
	type t = {
		thread_idx: Vec3.t;
		locals: Environ.t;
		mode: Exp.mode;
	}

	let mk ~thread_idx:tid ~locals:locals ~mode:mode =
		{thread_idx=tid; locals=locals; mode=mode}

	let to_json (x:t) : json =
		let open Yojson.Basic in
		`Assoc [
			"threadIdx", Vec3.to_json x.thread_idx;
			"locals", Environ.to_json x.locals;
			"mode", `String (match x.mode with R -> "rw" | W -> "rd");
		]

	let to_string (v:t) : string =
		let open Yojson.Basic in
		to_json v |> pretty_to_string

end

module Witness = struct
	type t = {
		indices : string list;
		tasks : Task.t * Task.t;
		block_idx: Vec3.t;
		block_dim: Vec3.t;
		grid_dim: Vec3.t;
		globals: Environ.t;
	}

	let to_json (x:t) : json =
		let open Yojson.Basic in
		let (t1, t2) = x.tasks in
		`Assoc [
			"task1", Task.to_json t1;
			"task2", Task.to_json t2;
			"blockDim", Vec3.to_json x.block_dim;
			"blockIdx", Vec3.to_json x.block_idx;
			"gridDim", Vec3.to_json x.grid_dim;
			"indices", `List (List.map (fun x -> `String x) x.indices);
			"globals", Environ.to_json x.globals;
		]

	let to_string (v:t) : string =
		let open Yojson.Basic in
		to_json v |> pretty_to_string


	let parse_vec3 (prefix:string) (globals:Environ.t) : Environ.t * Vec3.t =
		let (env, globals) = List.partition (fun (k, _) -> String.starts_with ~prefix:(prefix ^ ".") k) globals in
		let get (x:string) : string =
			Environ.get (prefix ^ "." ^ x) env |> Ojson.unwrap_or "?"
		in
		(globals, Vec3.{x = get "x"; y = get "y"; z = get "z";})

	let parse_indices (kvs:Environ.t) : string list =
		(* 
		$T1$idx$0: 1
		$T2$idx$0: 1
		*)
		(* get the maximum integer, in this case 0 *)
		let biggest_idx =
			List.split kvs
			|> fst
			|> List.filter (fun k -> Common.contains ~needle:"$idx$" k)
			|> List.map (fun k ->
					match Common.rsplit '$' k with
					| Some (_, idx) -> int_of_string idx
					| None -> failwith "unexpected"
			)
			|> List.fold_left Int.max 0
		in
		(* Parse a single index, in this case 1 *)
		let parse_idx (idx:string) : string =
			let parse (prefix:string) : string option =
				List.assoc_opt ("$T" ^ prefix ^ "$idx$" ^ idx) kvs
			in
			match parse "1" with
			| Some v -> v
			| None ->
				(match parse "2" with
					| Some v -> v
					| None -> failwith "Index malformed!")
		in
		(* Range over all indices *)
		Common.range biggest_idx
		(* Convert them to strings *)
		|> List.map string_of_int
		(* And look them up using parse_idx *)
		|> List.map parse_idx 

	let parse_mode (kvs:Environ.t) : Exp.mode * Exp.mode =
		let parse (x:string) =
			match List.assoc ("$T" ^ x ^ "$mode") kvs with
			| "1" -> Exp.W
			| "0" -> Exp.R
			| _ -> failwith ("Unknown mode: " ^ x)
		in
		try
			(parse "1", parse "2")
		with
			Failure(e) ->
				List.iter (fun (k,v) -> print_endline (k ^ ": " ^ v)) kvs;
				failwith e

	let parse_meta (env:Environ.t) : (Environ.t * (string list * Exp.mode * Exp.mode)) =
		let (kvs, env) = List.partition (fun (k, _) ->
				String.starts_with ~prefix:"$" k
		) env
		in
		let (t1_mode, t2_mode) = parse_mode kvs in
		(env, (parse_indices kvs, t1_mode, t2_mode)) 

	let parse (m:Model.model) : t =
		let env = Environ.parse m in
		(* put all special variables in kvs
			$T2$loc: 0
			$T1$mode: 0
			$T1$loc: 1
			$T2$mode: 1
			$T1$idx$0: 1
			$T2$idx$0: 1
		*)
		let (env, (idx, t1_mode, t2_mode)) = parse_meta env in
		let (tids, env) = List.partition (fun (k, _) ->
				String.starts_with ~prefix:"threadIdx." k
		) env
		in
		let (locals, globals) = List.partition (fun (k, _) ->
			String.contains k '$'
		) env
		in
		let t1_locals, t2_locals = List.partition (fun (k, _) ->
			String.ends_with ~suffix:"$T1" k
		) locals
		in
		let (globals, block_idx) = parse_vec3 "blockIdx" globals in
		let (globals, block_dim) = parse_vec3 "blockDim" globals in
		let (globals, grid_dim) = parse_vec3 "gridDim" globals in

		let fix_locals (x:string) : string =
			match Common.rsplit '$' x with
			| Some (x, _) -> x
			| None -> x
		in
		let fix_locals : (string * string) list -> (string * string) list =
			List.map (fun (k, v) -> (fix_locals k, v))
		in
		let t1_locals = fix_locals t1_locals in
		let t2_locals = fix_locals t2_locals in
		let (t1_tid, t2_tid) = Vec3.parse tids in
		let t1 = Task.{
			thread_idx = t1_tid;
			locals = t1_locals;
			mode = t1_mode;
		} in
		let t2 = Task.{
			thread_idx = t2_tid;
			locals = t2_locals;
			mode = t2_mode;
		}
		in
		{
			block_idx = block_idx;
			block_dim = block_dim;
			grid_dim = grid_dim;
			indices = idx;
			tasks = t1, t2;
			globals = globals;
		}
end

module Solution = struct
	type t =
	| Drf
	| Racy of Witness.t
	| Unknown
	let to_json: t -> json =
		function
		| Drf -> `String "drf"
		| Unknown -> `String "unknown"
		| Racy w -> Witness.to_json w

	(*
		Example of retrieving values from a model.

		https://github.com/icra-team/icra/blob/ee3fd360ee75490277dd3fd05d92e1548db983e4/duet/pa/paSmt.ml
	 *)
	let solve ((cache, ps):(Symbexp.LocationCache.t * Symbexp.proof Streamutil.stream)) : t Streamutil.stream =
		Streamutil.map (fun p ->
			let ctx = Z3.mk_context [
				("model", "true");
				("proof", "false");
				("timeout", "500");
			] in
			let s = Solver.mk_simple_solver ctx in
			add s ctx p;
			let r = match Solver.check s [] with
			| UNSATISFIABLE -> Drf
			| SATISFIABLE ->
				(match Solver.get_model s with
				| Some m -> Racy (Witness.parse m)
				| None -> failwith "INVALID")
			| UNKNOWN -> Unknown
			in
			r
		) ps

end

