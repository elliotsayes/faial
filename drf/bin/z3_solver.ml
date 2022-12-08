open Stage0
open Stage1
open Drf

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

type binop = Z3.context -> Expr.expr -> Expr.expr -> Expr.expr
type unop = Z3.context -> Expr.expr -> Expr.expr

(* We define an abstract module to handle numeric operations
	 so that we can support arbitrary backends. *)
module type NUMERIC_OPS = sig
	val mk_var: Z3.context -> string -> Expr.expr
	val mk_num: Z3.context -> int -> Expr.expr
	val mk_bit_and: binop
	val mk_bit_or: binop
	val mk_bit_xor: binop
	val mk_left_shift: binop
	val mk_right_shift: binop
	val mk_plus: binop
	val mk_minus: binop
	val mk_mult: binop
	val mk_div: binop
	val mk_mod: binop
	val mk_le: binop
	val mk_ge: binop
	val mk_gt: binop
	val mk_lt: binop
  val parse_num: string -> string
end

module ArithmeticOps : NUMERIC_OPS = struct
	let missing (name:string) : binop = fun _ _ _ -> raise (Not_implemented name)
	let mk_var = Integer.mk_const_s
	let mk_num = Arithmetic.Integer.mk_numeral_i
	let mk_bit_and = missing "&"
	let mk_bit_or = missing "|"
	let mk_bit_xor =  missing "^"
	let mk_left_shift = missing "<<"
	let mk_right_shift = missing ">>"
	let mk_plus ctx n1 n2 = Arithmetic.mk_add ctx [n1; n2]
	let mk_minus ctx n1 n2 = Arithmetic.mk_sub ctx [n1; n2]
	let mk_mult ctx n1 n2 = Arithmetic.mk_mul ctx [n1; n2]
	let mk_div = Arithmetic.mk_div
	let mk_mod = Arithmetic.Integer.mk_mod
	let mk_le = Arithmetic.mk_le
	let mk_ge = Arithmetic.mk_ge
	let mk_gt = Arithmetic.mk_gt
	let mk_lt = Arithmetic.mk_lt
	let parse_num (x:string) = x
end


module BitVectorOps : NUMERIC_OPS = struct
	let word_size = 32
	let mk_var ctx x = BitVector.mk_const_s ctx x word_size
	let mk_num ctx n = BitVector.mk_numeral ctx (string_of_int n) word_size
	let mk_bit_and = BitVector.mk_and
	let mk_bit_or = BitVector.mk_or
	let mk_bit_xor =  BitVector.mk_xor
	let mk_left_shift = BitVector.mk_shl
	let mk_right_shift = BitVector.mk_ashr
	let mk_minus = BitVector.mk_sub
	let mk_plus = BitVector.mk_add
	let mk_mult = BitVector.mk_mul
	let mk_div = BitVector.mk_sdiv
	let mk_mod = BitVector.mk_smod
	let mk_le = BitVector.mk_sle
	let mk_ge = BitVector.mk_sge
	let mk_gt = BitVector.mk_sgt
	let mk_lt = BitVector.mk_slt
	let parse_num x =
		(* Input is: #x0000004000000000 *)
		let offset n x = String.sub x n (String.length x - n) in
		(* We need to remove the prefix #x *)
		let x = offset 2 x in (* Removes the prefix: #x *)
		(* Then we need to remove the prefix 0s,
		   otherwise Int32.of_string doesn't like it *)
		let rec trim_0 x =
			if String.length x > 0 && String.get x 0 = '0'
			then trim_0 (offset 1 x)
			else x
		in
		(* Prefix it with a 0x so that Int64.of_string knows it's an hex *)
    let x = "0x" ^ trim_0 x in
		(* Finally, convert it into an int64 (signed),
	     and then render it back to a string, as this is for display only *)
    if x = "0x"
    then "0"
    else Int32.of_string x |> Int32.to_string
end


module CodeGen (N:NUMERIC_OPS) = struct 
	let parse_num = N.parse_num

	let nbin_to_expr (op:nbin) : Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = match op with
		| BitAnd -> N.mk_bit_and
		| BitOr -> N.mk_bit_or
		| BitXOr -> N.mk_bit_xor
		| LeftShift -> N.mk_left_shift
		| RightShift -> N.mk_right_shift
		| Plus -> N.mk_plus
		| Minus -> N.mk_minus
		| Mult -> N.mk_mult
		| Div -> N.mk_div
		| Mod -> N.mk_mod

	let nrel_to_expr : nrel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
		| NEq -> Boolean.mk_eq
		| NNeq -> fun ctx n1 n2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx n1 n2)
		| NLe -> N.mk_le
		| NGe -> N.mk_ge
		| NLt -> N.mk_lt
		| NGt -> N.mk_gt

	let brel_to_expr : brel -> Z3.context -> Expr.expr -> Expr.expr -> Expr.expr = function
		| BOr -> fun ctx b1 b2 -> Boolean.mk_or ctx [b1; b2]
		| BAnd -> fun ctx b1 b2 -> Boolean.mk_and ctx [b1; b2]

	let rec n_to_expr (ctx:Z3.context) (n:nexp) : Expr.expr = match n with
		| Var x -> Variable.name x |> N.mk_var ctx
		| Proj _ ->
		    let n : string = Serialize.PPrint.n_to_s n in
		    raise (Not_implemented ("n_to_expr: not implemented for Proj of " ^ n))
		| NCall _ ->
				failwith "b_to_expr: invoke Predicates.inline to remove predicates"
		| Num (n:int) -> N.mk_num ctx n
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
		| Pred _ -> failwith "b_to_expr: invoke Predicates.inline to remove predicates"
end

module IntGen = CodeGen (ArithmeticOps)
module BvGen = CodeGen (BitVectorOps)

let add
  ?(add_block_dim=false)
  (b_to_expr : Z3.context -> bexp -> Expr.expr)
  (s:Solver.solver)
  (ctx:Z3.context)
  (p:Symbexp.Proof.t)
:
  unit
=
  let assign x n =
    n_eq (Var (Variable.from_name x)) (Num n)
    |> b_to_expr ctx
	in
	let vars : Expr.expr list =
    p.decls
    |> List.map (fun (x:string) ->
      n_le (Var (Variable.from_name x)) (Num 2147483647)
      |> b_to_expr ctx
    )
	in
	vars
	@
  (if add_block_dim then [
    assign "blockDim.y" 1;
    assign "blockDim.z" 1;
  ]
  else [])
  @
  [
    b_to_expr ctx (Predicates.inline p.goal)
  ]
  |> Solver.add s

module Environ = struct
	type t = (string * string) list

	let to_json (env:t) : json =
		`Assoc (
			List.map (fun (k, v) -> (k, `String v)) env
		)

	let to_string (env:t) : string =
		let open Yojson.Basic in
		to_json env |> pretty_to_string

	let get: string -> t -> string option = List.assoc_opt

	let parse (parse_num:string -> string) (m:Model.model) : t =
		Model.get_const_decls m
		|> List.map (fun d ->
			let key: string = FuncDecl.get_name d |> Symbol.get_string in
			let e : string = FuncDecl.apply d []
				|> (fun e -> Model.eval m e true)
				|> Option.map Expr.to_string
				|> Ojson.unwrap_or "?"
			in
			(key, parse_num e)
		)

end

module Vec3 = struct
	type t = {x : string; y: string; z: string;}
	let mk ~x:x ~y:y ~z:z : t = {x=x; y=y; z=z}

	let default : t = {x="?"; y="?"; z="?"}

  let to_assoc (v:t) : (string * string) list =
    [
      "x", v.x;
      "y", v.y;
      "z", v.z;
    ]

    let to_json (v:t) : json =
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
		location: Location.t option;
	}

	let mk ~thread_idx:tid ~locals:locals ~mode:mode ~location:location =
		{thread_idx=tid; locals=locals; mode=mode; location=location}

	let to_json (x:t) : json =
		`Assoc [
			"threadIdx", Vec3.to_json x.thread_idx;
			"locals", Environ.to_json x.locals;
			"mode", `String (match x.mode with R -> "rw" | W -> "rd");
			"location", `String (
        x.location
        |> Option.map Location.repr
        |> Option.value ~default:"?"
      )
		]

	let to_string (v:t) : string =
		let open Yojson.Basic in
		to_json v |> pretty_to_string

end

module Witness = struct
  type t = {
    proof_id: int;
    indices : string list;
    tasks : Task.t * Task.t;
    block_idx: Vec3.t;
    block_dim: Vec3.t;
    grid_dim: Vec3.t;
    globals: Environ.t;
  }

	let to_json (x:t) : json =
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


	let parse_vec3 (d:Vec3.t) (prefix:string) (globals:Environ.t) : Environ.t * Vec3.t =
		let (env, globals) = List.partition (fun (k, _) -> String.starts_with ~prefix:(prefix ^ ".") k) globals in
		let get ~default (x:string) : string =
			let z = match Environ.get (prefix ^ "." ^ x) env with
			| Some x -> x
			| None -> default
			in
			z
		in
		let v = Vec3.{
			x = get ~default:d.x "x";
			y = get ~default:d.y "y";
			z = get ~default:d.z "z";
		} in
		(globals, v)

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

	let parse (parse_location:int -> Location.t) (parse_num:string -> string) ~proof_id ~block_dim ~grid_dim (m:Model.model) : t =
		let env = Environ.parse parse_num m in
    let parse_loc (tid:string) =
      env
      |> Environ.get ("$T" ^ tid ^ "$loc")
      |> Option.map (fun x ->
        x
        |> int_of_string
        |> parse_location
      )
    in
    let t1_loc = parse_loc "1" in
    let t2_loc = parse_loc "2" in
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
		let (globals, block_idx) = parse_vec3 Vec3.default "blockIdx" globals in
		let (globals, block_dim) = parse_vec3 block_dim "blockDim" globals in
		let (globals, grid_dim) = parse_vec3 grid_dim "gridDim" globals in

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
      location = t1_loc;
		} in
		let t2 = Task.{
			thread_idx = t2_tid;
			locals = t2_locals;
			mode = t2_mode;
      location = t2_loc;
		}
		in
		{
      proof_id = proof_id;
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
	let solve
    ?(timeout=None)
    ?(show_proofs=false)
    ~block_dim
    ~grid_dim
    (ps:Symbexp.Proof.t Streamutil.stream)
  :
    t Streamutil.stream
  =
    let b_to_expr = ref IntGen.b_to_expr in
    let parse_num = ref IntGen.parse_num in
    Streamutil.map (fun p ->
      let options = [
      ("model", "true");
        ("proof", "false");
      ] @
      begin match timeout with
        | Some timeout -> ["timeout", string_of_int timeout]
        | None -> []
      end
      in
      let ctx = Z3.mk_context options in
			let s =
				(* Create a solver and try to solve, might fail with Not_Implemented *)
				let solve b_to_expr =
					let s = Solver.mk_simple_solver ctx in
					add ~add_block_dim:(Option.is_none block_dim) b_to_expr s ctx p;
					s
				in
				try
          solve !b_to_expr
				with
					Not_implemented x ->
						prerr_endline ("WARNING: arithmetic solver cannot handle operator '" ^ x ^ "', trying bit-vector arithmetic instead.");
						b_to_expr := BvGen.b_to_expr;
						parse_num := BvGen.parse_num;
						solve !b_to_expr
			in
			(if show_proofs then (
        let title = "proof #" ^ string_of_int p.id in
        let body = Solver.to_string s ^ "(check-sat)\n(get-model)\n" in
        Tui.print_frame ~title ~body
      ) else ());
      let block_dim = block_dim |> Ojson.unwrap_or Vec3.default in
      let grid_dim = grid_dim |> Ojson.unwrap_or Vec3.default in
			let r = match Solver.check s [] with
			| UNSATISFIABLE -> Drf
			| SATISFIABLE ->
				(match Solver.get_model s with
				| Some m -> Racy (Witness.parse (List.nth p.locations) !parse_num ~block_dim ~grid_dim ~proof_id:p.id m)
				| None -> failwith "INVALID")
			| UNKNOWN -> Unknown
			in
			r
		) ps

end

