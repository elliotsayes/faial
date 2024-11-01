open Protocols

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
  | RelMinus

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
  | "-" -> Some RelMinus
  | _ -> None

type 'a unop = {op: 'a; arg: D_lang.Expr.t}

type t = {
  name: Variable.t;
  init: D_lang.Expr.t;
  cond: comparator unop;
  inc: increment unop;
}

let parse_init: D_lang.ForInit.t option -> (Variable.t * D_lang.Expr.t) option =
  function
  | Some (Decls ({var=n; init=Some (IExpr i); _}::_)) -> Some (n, i)
  | Some (Expr (BinaryOperator {lhs=Ident d; opcode="="; rhs=i; _})) -> Some (d.name, i)
  | _ -> None

let parse_cond (c:D_lang.Expr.t option) : (Variable.t * comparator unop) option =
  match c with
  | Some (BinaryOperator {lhs=Ident d; opcode=o; rhs=r; _}) ->
    (match parse_cmp o with
    | Some o -> Some (d.name, {op=o; arg=r})
    | _ -> None)
  | _ -> None

let rec parse_inc (i:D_lang.Expr.t option) : (Variable.t * increment unop) option =
  match i with
  | Some (BinaryOperator {opcode=","; lhs=l; _}) ->
    parse_inc (Some l)
  | Some (BinaryOperator {
      lhs = Ident l;
      opcode = "=";
      rhs = BinaryOperator (
          {lhs=Ident l'; opcode=o; rhs=r; _}
        | {lhs=r; opcode=o; rhs=Ident l'; _}
      );
      _
    }) ->
    begin
      let l = l.name in
      match
        parse_inc_op o
      with
      | Some o when Variable.equal l l'.name ->
        Some (l, {op=o; arg=r})
      | _ -> None
    end
  | _ -> None


let from_for (loop: D_lang.Stmt.d_for) : t option =
  let ( let* ) = Option.bind in
  let* (x1, inc) = parse_inc loop.inc in
  let* init =
    match parse_init loop.init with
    | Some (x2, init) -> if Variable.equal x1 x2 then Some init else None
    | None -> Some (D_lang.Expr.ident x1)
  in
  let* (_, cond) = parse_cond loop.cond in
  Some {
    name = x1;
    init = init;
    cond = cond;
    inc = inc;
  }

let from_while (loop: D_lang.Stmt.d_cond) : (t * D_lang.Stmt.t) option =
  let ( let* ) = Option.bind in
  let (body, inc) = D_lang.Stmt.last loop.body in
  let inc = match inc with
    | SExpr inc ->
      Some inc
    | _ -> None
  in
  let* (x1, inc) = parse_inc inc in
  let init = D_lang.Expr.ident x1 in
  let* (_, cond) = parse_cond (Some loop.cond) in
  Some ({
    name = x1;
    init = init;
    cond = cond;
    inc = inc;
  }, body)

let unop_to_s (f:'a -> string) (u:'a unop) : string =
  f u.op ^ " " ^ D_lang.Expr.to_string u.arg

let opt_to_s (f:'a -> string) : 'a option -> string =
  function
  | Some x -> f x
  | None -> ""


let cmp_to_s : comparator -> string =
  function
  | Lt -> "<"
  | Gt -> ">"
  | LtEq -> "<="
  | GtEq -> ">="
  | RelMinus -> "-"

let inc_to_s : increment -> string =
  function
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
