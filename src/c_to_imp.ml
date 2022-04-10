open Exp

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

type c_error =
| RootCause of string
| Because of string * c_error

type 'a c_result = ('a, c_error) Result.t

let root_cause (msg:string) : 'a c_result =
  Error (RootCause msg)

let parse_nbin: string -> nbin =
  function
  | "+" -> Plus
  | "-" -> Minus
  | "*"  -> Mult
  | "/" -> Div
  | "%" -> Mod
  | ">>" -> RightShift
  | "<<" -> LeftShift
  | "^" -> BitXOr
  | "|" -> BitOr
  | "&" -> BitAnd
  | x ->
    prerr_endline ("WARNING: parse_nbin: Can't handle '(int, int) int' operator '"^ x ^"' converting it to +");
    Plus

let parse_nrel_opt: string -> nrel option =
  function
  | "==" -> Some NEq
  | "!=" -> Some NNeq
  | "<=" -> Some NLe
  | "<"  -> Some NLt
  | ">=" -> Some NGe
  | ">"  -> Some NGt
  | x -> None

let parse_brel: string -> bexp -> bexp -> bexp =
  function
  | "||" -> b_or
  | "&&" -> b_and
  | x ->
    prerr_endline ("WARNING: parse_brel: Can't handle " ^ x ^ " converting it to |");
    b_or

let with_exp (msg:string) (e: Cast.c_exp) (f:Cast.c_exp -> 'a c_result) (c:Cast.c_exp): 'a c_result =
  match f c with
  | Ok o -> Ok o
  | Error err -> Error (Because (msg ^ ": " ^ Cast.exp_to_s e, err))

let parse_var: Cast.c_exp -> variable c_result =
  function
  | NonTypeTemplateParmDecl { name = v ; _ }
  | ParmVarDecl { name = v ; _ }
  | VarDecl { name = v ; _ }
  | FunctionDecl { name = v; _ } -> Ok v
  | e -> root_cause ("parse_var: unexpected expression: " ^ Cast.exp_to_s e)

let rec parse_nexp (e: Cast.c_exp) : nexp c_result =
  let parse_b m b = with_exp m e parse_bexp b in
  let parse_n m n = with_exp m e parse_nexp n in
  match e with
  | IntegerLiteral n
  | CharacterLiteral n -> Ok (Num n)
  | FloatingLiteral n -> 
    prerr_endline ("WARNING: parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
    Ok (Num (Float.to_int n))
  | ConditionalOperator o ->
    let* b = parse_b "cond" o.cond in
    let* n1 = parse_n "then_expr" o.then_expr in
    let* n2 = parse_n "else_expr" o.else_expr in
    Ok (n_if b n1 n2)
  | CallExpr {func = FunctionDecl {name = n; _}; args = [n1; n2]} when var_name n = "min" ->
    let* n1 = parse_n "lhs" n1 in
    let* n2 = parse_n "rhs" n2 in
    Ok (n_if (n_lt n1 n2) n1 n2)
  | CallExpr {func = FunctionDecl {name = n; _}; args = [n1; n2]} when var_name n = "max" ->
    let* n1 = parse_n "lhs" n1 in
    let* n2 = parse_n "rhs" n2 in
    Ok (n_if (n_gt n1 n2) n1 n2)
  | BinaryOperator {opcode=o; lhs=n1; rhs=n2} ->
    let* n1 = parse_n "lhs" n1 in
    let* n2 = parse_n "rhs" n2 in
    Ok (n_bin (parse_nbin o) n1 n2)
  | _ ->
    prerr_endline ("WARNING: rewriting the following expression as 0: " ^ Cast.exp_to_s e);
    Ok (Num 0)
and parse_bexp (e: Cast.c_exp) : bexp c_result =
  let parse_b m b = with_exp m e parse_bexp b in
  let parse_n m n = with_exp m e parse_nexp n in
  match e with
  | BinaryOperator o ->
    (match parse_nrel_opt o.opcode with
    | Some r ->
      let* n1 = parse_n "lhs" o.lhs in
      let* n2 = parse_n "rhs" o.rhs in
      Ok (n_rel r n1 n2)
    | None ->
      let* b1 = parse_b "lhs" o.lhs in
      let* b2 = parse_b "rhs" o.rhs in
      Ok (parse_brel o.opcode b1 b2)
    )
  | IntegerLiteral i ->
    Ok (Bool (i != 0))
  | UnaryOperator u when u.opcode == "!" ->
    let* b = parse_b "child" u.child in
    Ok (b_not b)
  | _ -> root_cause ("parse_bexp: unknown expression: " ^ Cast.exp_to_s e)
  

(*
let parse_task = function
  | 0 -> Ok Task1
  | 1 -> Ok Task2
  | n -> root_cause ("parse_task: expected 0 or 1, but got " ^ string_of_int n)
*)