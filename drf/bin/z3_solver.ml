open Stage0
open Protocols
open Drf
open Gen_z3
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
		mode: Access.Mode.t;
		location: Location.t option;
	}

	let mk ~thread_idx:tid ~locals:locals ~mode:mode ~location:location =
		{thread_idx=tid; locals=locals; mode=mode; location=location}

	let to_json (x:t) : json =
		`Assoc [
			"threadIdx", Vec3.to_json x.thread_idx;
			"locals", Environ.to_json x.locals;
			"mode", `String (Access.Mode.to_string x.mode);
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
			|> List.filter (fun k -> Common.contains ~substring:"$idx$" k)
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

	let parse_mode (kvs:Environ.t) : Access.Mode.t * Access.Mode.t =
		let parse (x:string) =
      let open Access.Mode in
			match List.assoc ("$T" ^ x ^ "$mode") kvs with
			| "1" -> Wr
			| "0" -> Rd
			| _ -> failwith ("Unknown mode: " ^ x)
		in
		try
			(parse "1", parse "2")
		with
			Failure(e) ->
				List.iter (fun (k,v) -> print_endline (k ^ ": " ^ v)) kvs;
				failwith e

	let parse_meta (env:Environ.t) : (Environ.t * (string list * Access.Mode.t * Access.Mode.t)) =
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
    ?(logic=None)
    ~block_dim
    ~grid_dim
    (ps:Symbexp.Proof.t Streamutil.stream)
  :
    t Streamutil.stream
  =
    let b_to_expr = ref IntGen.b_to_expr in
    let parse_num = ref IntGen.parse_num in
    logic |> Option.iter (fun l ->
      if String.ends_with ~suffix:"BV" l then (
        prerr_endline ("WARNING: user set bit-vector logic " ^ l);
        b_to_expr := BvGen.b_to_expr;
        parse_num := BvGen.parse_num;
      ) else ()
    );
    let logic = ref logic in
    let set_bv () : unit =
      prerr_endline ("WARNING: using bit-vector logic.");
      b_to_expr := BvGen.b_to_expr;
      parse_num := BvGen.parse_num;
      logic := None;
    in
    (* Logic is bit-vector based *)
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
      let s =
        (* Create a solver and try to solve, might fail with Not_Implemented *)
        let solve () =
          let ctx = Z3.mk_context options in
          let s =
            match !logic with
              | None ->
                Solver.mk_simple_solver ctx
              | Some logic ->
                Solver.mk_solver_s ctx logic
          in
          add ~add_block_dim:(Option.is_none block_dim) !b_to_expr s ctx p;
          s
        in
        try
          solve ()
        with
          Not_implemented x ->
            prerr_endline ("WARNING: arithmetic solver cannot handle operator '" ^ x ^ "', trying bit-vector arithmetic instead.");
            set_bv ();
            solve ()
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

