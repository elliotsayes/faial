module StackTrace = Common.StackTrace

open Exp

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

type c_error = string StackTrace.t

let print_error : c_error -> unit =
  StackTrace.iter prerr_endline

let error_to_buffer (e: c_error) : Buffer.t =
  let b = Buffer.create 512 in
  StackTrace.iter (Buffer.add_string b) e;
  b

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

let with_msg (msg:string) (f:'a -> 'b c_result) (c:'a): 'b c_result =
  match f c with
  | Ok o -> Ok o
  | Error err -> Error (Because (msg, err))

let with_exp (msg:string) (e: Cast.c_exp) : (Cast.c_exp -> 'a c_result) -> Cast.c_exp -> 'a c_result =
  with_msg (msg ^ ": " ^ Cast.exp_to_s e)

let parse_var: Cast.c_exp -> variable c_result =
  function
  | NonTypeTemplateParmDecl { name = v ; _ }
  | ParmVarDecl { name = v ; _ }
  | VarDecl { name = v ; _ }
  | FunctionDecl { name = v; _ } -> Ok v
  | e -> root_cause ("parse_var: unexpected expression: " ^ Cast.exp_to_s e)

let is_variable : Cast.c_exp -> bool =
  function
  | NonTypeTemplateParmDecl _
  | ParmVarDecl _
  | VarDecl _
    -> true
  | _ -> false

type c_access = {location: variable; mode: mode; index: Cast.c_exp list }
(*
let access_to_s (a:c_access) : string =
  let mode = match a.mode with
  | R -> "ro "
  | W -> "rw "
  in
  mode ^ exp_to_s a.location ^ "[" ^ list_to_s exp_to_s a.index ^ "]"
*)
type c_location_alias = {
  source: Cast.c_exp;
  target: Cast.c_exp;
  offset: Cast.c_exp
}

let rec parse_nexp (e: Cast.c_exp) : nexp c_result =
  let parse_b m b = with_exp m e parse_bexp b in
  let parse_n m n = with_exp m e parse_nexp n in
  match e with
  | NonTypeTemplateParmDecl { name = v ; _ }
  | ParmVarDecl { name = v ; _ }
  | VarDecl { name = v ; _ }
    -> Ok (Var v)
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
  | CXXBoolLiteralExpr b ->
    Ok (Num (if b then 1 else 0))
  | UnaryOperator {opcode="~"; _}
  | DeclRefExpr _
  | MemberExpr _
  | FunctionDecl _ 
  | ArraySubscriptExpr _
  | CallExpr _ ->
    prerr_endline ("WARNING: parse_nexp: rewriting to unknown: " ^ Cast.exp_to_s e);
    Ok NUnknown

  | _ ->
    root_cause ("WARNING: parse_nexp: unsupported expression " ^ Cast.exp_name e ^ " : " ^ Cast.exp_to_s e)

and parse_bexp (e: Cast.c_exp) : bexp c_result =
  let parse_b m b = with_exp ("parse_bexp: " ^ m) e parse_bexp b in
  let parse_n m n = with_exp ("parse_bexp: " ^ m) e parse_nexp n in
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

  | UnaryOperator u when u.opcode = "!" ->
    let* b = parse_b "not" u.child in
    Ok (b_not b)

  | PredicateExpr p ->
    let* n = parse_n "child" p.child in
    Ok (Pred(p.opcode, n))

  | _ ->
    let* n = parse_n (Cast.exp_name e) e in
    prerr_endline ("WARNING: parse_bexp: rewriting '" ^ Cast.exp_to_s e ^ "' to: 0 != " ^ Cast.exp_to_s e);
    Ok (n_neq n (Num 0))

let j_parse_nexp (j:Yojson.Basic.t) : nexp =
  match Cast.parse_exp j with
  | Ok e ->
    (match parse_nexp e with
    | Ok e -> e
    | Error e -> raise (Common.ParseError (error_to_buffer e))
    )
  | Error e ->
    raise (Common.ParseError (Rjson.error_to_buffer e))

let parse_range (r:Cast.c_range) : Exp.range c_result =
  let parse_n m b = with_msg (m ^ ": " ^ Cast.range_to_s r) parse_nexp b in
  let* lb = parse_n "lower_bound" r.lower_bound in
  let* ub = parse_n "upper_bound" r.upper_bound in
  let* s = match r.opcode with
    | "+" ->
      let* n = parse_n "step" r.step in
      Ok (Default n)
    | s -> Ok (StepName s)
  in
  Ok {
    range_var = r.name;
    range_step = s;
    range_lower_bound = lb;
    range_upper_bound = ub;
  }

let make_access (m:Exp.mode) (c:Cast.c_array_subscript) : c_access option =
  let rec make_access (c:Cast.c_array_subscript) (indices:Cast.c_exp list) : c_access option =
    let indices = c.rhs :: indices in
    match c.lhs with
    | ArraySubscriptExpr a ->
      make_access a indices
    | VarDecl {name=n; _}
    | ParmVarDecl {name=n; _} ->
      let open Cast in
      Some {location=n; mode=m; index=indices}
    | _ -> None
  in
  make_access c []

let rec get_accesses (c:Cast.c_exp) : c_access list =
  match c with

  | CXXOperatorCallExpr {
      func=CXXMethodDecl{name=v; _};
      args=[ArraySubscriptExpr a; e]
    } when var_name v = "operator="
    ->
    from_access W a @ get_accesses e

  | BinaryOperator {lhs=ArraySubscriptExpr a; rhs=r; opcode="="; _} ->
    from_access W a @ get_accesses r

  | ArraySubscriptExpr a -> from_access R a

  | BinaryOperator ({lhs=MemberExpr {base=e; _}; rhs=r; opcode="="; _} as b) ->
    get_accesses (BinaryOperator {b with lhs=e})

  | CXXOperatorCallExpr {func=f; args=args}
  | CallExpr {func=f; args=args} ->
    get_accesses f @ List.concat_map get_accesses args

  | UnaryOperator {child=e; _}
  | PredicateExpr {child=e; _}
  | MemberExpr {base=e; _} ->
    get_accesses e

  | BinaryOperator {lhs=l; rhs=r; _} -> get_accesses l @ get_accesses r

  | ConditionalOperator {cond=e1; then_expr=e2; else_expr=e3} ->
    get_accesses e1 @ get_accesses e2 @ get_accesses e3

  | VarDecl _
  | UnresolvedLookupExpr _
  | ParmVarDecl _
  | DeclRefExpr _
  | FloatingLiteral _
  | FunctionDecl _
  | IntegerLiteral _
  | CXXMethodDecl _
  | NonTypeTemplateParmDecl _
  | CXXBoolLiteralExpr _
  | CharacterLiteral _
    -> []

  and from_access mode a =
    match make_access mode a with
    | Some a -> a::get_accesses_list a.index
    | None -> []

  and get_accesses_list (l:Cast.c_exp list) =
    match l with
    | [] -> []
    | n :: l -> get_accesses n @ get_accesses_list l

let cast_map f = Rjson.map_all f (fun idx s e ->
  StackTrace.Because ("Error in index #" ^ (string_of_int (idx + 1)), e))

let parse_access (a:c_access) : Exp.acc_expr c_result =
  let* i = with_msg "parse_access: index" (cast_map parse_nexp) a.index in
  Ok (a.location, {access_index=i; access_mode=a.mode})


let parse_decl (d:Cast.c_decl) : (variable * Imp.locality * nexp option) option c_result =
  let parse_n m b = with_msg (m ^ ": " ^ Cast.decl_to_s d) parse_nexp b in
  let* ty = match Cast.parse_type d.ty with
  | Ok ty -> Ok ty
  | Error _ -> root_cause ("parse_decl: error parsing type: " ^ Rjson.pp_js d.ty)
  in
  if Ctype.is_int ty
  then (
    let* (n:Exp.nexp option) = match d.init with
    | Some (IExp n) ->
      let* n = parse_n "init" n in
      Ok (Some n)
    | _ -> Ok None
    in
    Ok (Some (d.name, Imp.Local, n))
  )
  else Ok None

let parse_accesses (e:Cast.c_exp) : (Imp.stmt list) c_result =
  let with_msg (m:string) f b = with_msg ("parse_accesses: " ^ m) f b in
  let* accs = with_msg "get_accesses" (cast_map parse_access) (get_accesses e) in
  let c = List.length accs in
  let msg = if c = 0 then
    "skip"
  else
    string_of_int c ^ " accesses found in"
  in
  prerr_endline (
    "WARNING: parse_stmt: " ^ msg ^ " expression: " ^
    Cast.exp_to_s e
  );
  Ok (List.map (fun a -> Imp.Acc a) accs)

let is_pointer (j:Yojson.Basic.t) =
  match Cast.parse_type j with
  | Ok t -> Ctype.is_pointer t
  | Error _ -> false

let rec parse_load_expr (target:Cast.c_exp) (exp:Cast.c_exp)
  : (c_location_alias, Cast.c_exp) Either.t =
  let open Imp in
  let open Either in
  match exp with
  | VarDecl {ty=ty; _}
  | ParmVarDecl {ty=ty; _} when is_pointer ty ->
    Left {source=exp; target=target; offset=IntegerLiteral 0}
  | BinaryOperator ({lhs=l; rhs=r; _} as b) ->
    (match parse_load_expr target l with
    | Left l -> Left {l with offset =BinaryOperator {b with lhs=l.offset}}
    | Right _ -> Right exp)
  | _ ->
    Right exp

let parse_location_alias (s:c_location_alias) : Imp.stmt c_result =
  let* source = with_msg "location_alias.source" parse_var s.source in
  let* target = with_msg "location_alias.target" parse_var s.target in
  let* offset = with_msg "location_alias.offset" parse_nexp s.offset in
  let open Imp in
  Ok (LocationAlias {
    alias_source=source;
    alias_target=target;
    alias_offset=offset
  })


let rec parse_stmt (c:Cast.c_stmt) : Imp.stmt list c_result =
  let with_msg (m:string) f b = with_msg ("parse_stmt: " ^ m) f b in
  let parse_accesses_opt = function
  | None -> Ok []
  | Some e -> parse_accesses e
  in
  match c with
  | BreakStmt
  | GotoStmt
  | ReturnStmt -> Ok []

  | IfStmt c ->
    let* b = with_msg "if.cond" parse_bexp c.cond in
    let* t = with_msg "if.then" parse_stmt c.then_stmt in
    let* e = with_msg "if.else" parse_stmt c.else_stmt in
    let open Imp in
    Ok [s_if b (Block t) (Block e)]

  | CompoundStmt l ->
    let* l = cast_map parse_stmt l in
    Ok (List.concat l)

  | DeclStmt l ->
    let* l = cast_map parse_decl l |> Result.map Common.flatten_opt in
    Ok [Imp.Decl l]

  | SExp ((BinaryOperator {opcode="="; lhs=VarDecl {name=v; ty=ty} as lhs; rhs=rhs}) as orig)
  | SExp ((BinaryOperator {opcode="="; lhs=ParmVarDecl {name=v; ty=ty} as lhs; rhs=rhs}) as orig)
    when is_pointer ty
    ->
    (match parse_load_expr lhs rhs with
    | Left a ->
      let* a = parse_location_alias a in
      Ok [a]
    | Right _ -> 
      let* accs = with_msg "location_alias" parse_accesses orig in
      Ok accs
    )

  | SExp (BinaryOperator {opcode="="; lhs=VarDecl {name=v; ty=ty}; rhs=rhs})
  | SExp (BinaryOperator {opcode="="; lhs=ParmVarDecl {name=v; ty=ty}; rhs=rhs})
    ->
    let* accs = with_msg "assign.accs" parse_accesses rhs in
    let* rhs = with_msg "assign.rhs" parse_nexp rhs in
    let open Imp in 
    Ok (accs @ [Decl [v, Local, Some rhs]])

  | ForEachStmt s ->
    let* r = with_msg "foreach.range" parse_range s.range in
    let* b = with_msg "foreach.body" parse_stmt s.body in
    let open Imp in
    Ok [For (r, Block b)]

  | SyncStmt -> Ok [Imp.Sync]

  | AssertStmt b ->
    let* b = with_msg "assert.cond" parse_bexp b in
    Ok [Imp.Assert b]

  | SExp e ->
    let* accs = with_msg "SExp" parse_accesses e in
    Ok accs

  | ForStmt s ->
    let* b = with_msg "for.body" parse_stmt s.body in
    let get_accs msg e = with_msg ("for." ^ msg) parse_accesses_opt e in
    let* accs1 = get_accs "init" s.init in
    let* accs2 = get_accs "cond" s.cond in
    let* accs3 = get_accs "inc" s.inc in
    let open Imp in
    Ok (accs1 @ accs2 @ accs3 @ [Loop (Block b)])

  | DoStmt {cond=cond; body=body} ->
    let* body = with_msg "do.body" parse_stmt body in
    let* accs = with_msg "do.cond" parse_accesses cond in
    let open Imp in
    Ok (Loop (Block body) :: accs)

  | WhileStmt {cond=cond; body=body} ->
    let* body = with_msg "while.body" parse_stmt body in
    let* accs = with_msg "while.cond" parse_accesses cond in
    let open Imp in
    Ok (accs @ [Loop (Block body)])

  | SwitchStmt s ->
    let* accs = with_msg "switch.cond" parse_accesses s.cond in
    let* body = with_msg "switch.body" parse_stmt s.body in
    let open Imp in
    Ok (accs @ [Loop (Block body)])

  | CaseStmt s ->
    with_msg "case.body" parse_stmt s.body

  | DefaultStmt s ->
    with_msg "default.body" parse_stmt s

