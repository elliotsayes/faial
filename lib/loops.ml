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
	{op: 'a; arg: Dlang.d_exp}

type d_for_range = {
	name: variable;
	init: Dlang.d_exp;
	cond: comparator unop;
	inc: increment unop;
}

let parse_init: Dlang.d_for_init option -> (variable*Dlang.d_exp) option =
	function
	| Some (ForDecl ({name=n; init=Some (IExp i)}::_)) -> Some (n, i)
	| Some (ForExp (BinaryOperator {lhs=l; opcode="="; rhs=i})) ->
		(match Dlang.get_variable l with
		| Some v -> Some (v, i)
		| _ -> None)
	| _ -> None

let parse_cond (c:Dlang.d_exp option) : (variable * comparator unop) option =
	match c with
	| Some (BinaryOperator {lhs=l; opcode=o; rhs=r}) ->
		(match Dlang.get_variable l, parse_cmp o with
		| Some l, Some o -> Some (l, {op=o; arg=r})
		| _, _ -> None)
	| _ -> None

let rec parse_inc (i:Dlang.d_exp option) : (variable * increment unop) option =
	match i with
	| Some (BinaryOperator {opcode=","; lhs=l}) ->
		parse_inc (Some l)
	| Some (BinaryOperator {
			lhs=l;
			opcode="=";
			rhs=BinaryOperator{lhs=l'; opcode=o; rhs=r}
		}) ->
		begin
			match
				Dlang.get_variable l,
				Dlang.get_variable l',
				parse_inc_op o
			with
			| Some l, Some l', Some o when Variable.equal l l' ->
				Some (l, {op=o; arg=r})
			| _ -> None
		end
	| _ -> None


let parse_for (loop: Dlang.d_for) : d_for_range option =
	let (let*) = Option.bind in
	let* (x1, init) = parse_init loop.init in
	let* (x2, cond) = parse_cond loop.cond in
	let* (x3, inc) = parse_inc loop.inc in
	Some {
		name = x1;
		init = init;
		cond = cond;
		inc = inc;
	}


let unop_to_s (f:'a -> string) (u:'a unop) : string =
	f u.op ^ " " ^ Dlang.exp_to_s u.arg

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

let for_range_to_s (l:d_for_range) : string =
	"(" ^
		Variable.name l.name ^
		"= " ^ Dlang.exp_to_s l.init ^
		"; " ^ unop_to_s cmp_to_s l.cond ^
		"; " ^ unop_to_s inc_to_s l.inc ^
	")"
