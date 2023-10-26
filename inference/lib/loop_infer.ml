open Protocols

type variable = Variable.t

type increment =
  | Plus
  | LShift
  | Mult
	| Minus
	| RShift
	| Div

type comparator =
  | Lt
  | LtEq
  | Gt
  | GtEq

let parse_inc_op (o:string) : increment option =
  match o with
  | "-" -> Some Minus
  | "/" -> Some Div
  | ">>" -> Some RShift
	| "+" -> Some Plus
	| "<<" -> Some LShift
	| "*" -> Some Mult
	| _ -> None

let parse_cmp (o:string) : comparator option =
	match o with
	| "<" -> Some Lt
	| ">" -> Some Gt
	| "<=" -> Some LtEq
	| ">=" -> Some GtEq
	| _ -> None

type 'a unop =
	{op: 'a; arg: D_lang.Expr.t}

type t = {
	name: variable;
	init: D_lang.Expr.t;
	cond: comparator unop;
	inc: increment unop;
}

let parse_init: D_lang.ForInit.t option -> (variable*D_lang.Expr.t) option =
	function
	| Some (Decls ({ty_var={name=n; _}; init=Some (IExpr i); _}::_)) -> Some (n, i)
	| Some (Expr (BinaryOperator {lhs=l; opcode="="; rhs=i; _})) ->
		(match D_lang.Expr.to_variable l with
		| Some v -> Some (v, i)
		| _ -> None)
	| _ -> None

let parse_cond (c:D_lang.Expr.t option) : (variable * comparator unop) option =
	match c with
	| Some (BinaryOperator {lhs=l; opcode=o; rhs=r; _}) ->
		(match D_lang.Expr.to_variable l, parse_cmp o with
		| Some l, Some o -> Some (l, {op=o; arg=r})
		| _, _ -> None)
	| _ -> None

let rec parse_inc (i:D_lang.Expr.t option) : (variable * increment unop) option =
	match i with
	| Some (BinaryOperator {opcode=","; lhs=l; _}) ->
		parse_inc (Some l)
	| Some (BinaryOperator {
			lhs=l;
			opcode="=";
			rhs=BinaryOperator{lhs=l'; opcode=o; rhs=r; _};
      _
		}) ->
		begin
			match
				D_lang.Expr.to_variable l,
				D_lang.Expr.to_variable l',
				parse_inc_op o
			with
			| Some l, Some l', Some o when Variable.equal l l' ->
				Some (l, {op=o; arg=r})
			| _ -> None
		end
	| _ -> None


let from_for (loop: D_lang.Stmt.d_for) : t option =
	let (let*) = Option.bind in
	let* (x1, init) = parse_init loop.init in
	let* (_, cond) = parse_cond loop.cond in
	let* (_, inc) = parse_inc loop.inc in
	Some {
		name = x1;
		init = init;
		cond = cond;
		inc = inc;
	}


let unop_to_s (f:'a -> string) (u:'a unop) : string =
	f u.op ^ " " ^ D_lang.Expr.to_string u.arg

let opt_to_s (f:'a -> string) (o:'a option) : string =
	match o with
	| Some x -> f x
	| None -> ""


let cmp_to_s : comparator -> string = function
	| Lt -> "<"
	| Gt -> ">"
	| LtEq -> "<="
	| GtEq -> ">="

let inc_to_s : increment -> string = function
  | Plus -> "+"
  | LShift -> ">>"
  | Mult -> "*"
  | Minus -> "-"
  | RShift -> ">>"
  | Div -> "/"

let to_string (l:t) : string =
	"(" ^
		Variable.name l.name ^
		"= " ^ D_lang.Expr.to_string l.init ^
		"; " ^ unop_to_s cmp_to_s l.cond ^
		"; " ^ unop_to_s inc_to_s l.inc ^
	")"
