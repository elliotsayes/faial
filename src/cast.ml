module StackTrace = Common.StackTrace

open Exp
open Serialize
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

type c_type = json

type c_exp =
  | CharacterLiteral of int
  | ArraySubscriptExpr of c_array_subscript
  | BinaryOperator of c_binary
  | CallExpr of {func: c_exp; args: c_exp list; ty: c_type}
  | ConditionalOperator of {cond: c_exp; then_expr: c_exp; else_expr: c_exp; ty: c_type}
  | CXXConstructExpr of {args: c_exp list; ty: c_type}
  | CXXBoolLiteralExpr of bool
  | CXXMethodDecl of {name: variable; ty: c_type}
  | CXXOperatorCallExpr of {func: c_exp; args: c_exp list; ty: c_type}
  | FloatingLiteral of float
  | FunctionDecl of {name: variable; ty: c_type}
  | IntegerLiteral of int
  | NonTypeTemplateParmDecl of {name: variable; ty: c_type}
  | MemberExpr of {name: string; base: c_exp; ty: c_type}
  | ParmVarDecl of {name: variable; ty: c_type}
  | UnaryOperator of {opcode: string; child: c_exp; ty: c_type}
  | VarDecl of {name: variable; ty: c_type}
  | EnumConstantDecl of {name: variable; ty: c_type}
  | UnresolvedLookupExpr of {name: variable; tys: c_type list}
and c_binary = {opcode: string; lhs: c_exp; rhs: c_exp; ty: c_type}
and c_array_subscript = {lhs: c_exp; rhs: c_exp; ty: c_type}

let exp_name = function
| EnumConstantDecl _ -> "EnumConstantDecl"
| CharacterLiteral _ -> "CharacterLiteral"
| ArraySubscriptExpr _ -> "ArraySubscriptExpr"
| BinaryOperator _ -> "BinaryOperator"
| CallExpr _ -> "CallExpr"
| ConditionalOperator _ -> "ConditionalOperator"
| CXXBoolLiteralExpr _ -> "CXXBoolLiteralExpr"
| CXXConstructExpr _ -> "CXXConstructExpr"
| CXXMethodDecl _ -> "CXXMethodDecl"
| CXXOperatorCallExpr _ -> "CXXOperatorCallExpr"
| FloatingLiteral _ -> "FloatingLiteral"
| FunctionDecl _ -> "FunctionDecl"
| IntegerLiteral _ -> "IntegerLiteral"
| NonTypeTemplateParmDecl _ -> "NonTypeTemplateParmDecl"
| MemberExpr _ -> "MemberExpr"
| ParmVarDecl _ -> "ParmVarDecl"
| UnaryOperator _ -> "UnaryOperator"
| VarDecl _ -> "VarDecl"
| UnresolvedLookupExpr _ -> "UnresolvedLookupExpr"


type c_init =
  | InitListExpr of {ty: c_type; args: c_exp list}
  | IExp of c_exp

type c_decl = {
  name: variable;
  ty: c_type;
  init: c_init option;
  attrs: string list
}

type c_for_init =
  | ForDecl of c_decl list
  | ForExp of c_exp

type c_stmt =
  | BreakStmt
  | GotoStmt
  | ReturnStmt
  | IfStmt of {cond: c_exp; then_stmt: c_stmt; else_stmt: c_stmt}
  | CompoundStmt of c_stmt list
  | DeclStmt of c_decl list
  | WhileStmt of {cond: c_exp; body: c_stmt}
  | ForStmt of {init: c_for_init option; cond: c_exp option; inc: c_exp option; body: c_stmt}
  | DoStmt of {cond: c_exp; body: c_stmt}
  | SwitchStmt of {cond: c_exp; body: c_stmt}
  | DefaultStmt of c_stmt
  | CaseStmt of {case: c_exp; body: c_stmt}
  | SExp of c_exp

type c_param = {name: variable; is_used: bool; is_shared: bool; ty: c_type}

type c_kernel = {
  name: string;
  code: c_stmt;
  params: c_param list;
}


let rec exp_type (e:c_exp) : c_type =
  match e with
  | CXXConstructExpr c -> c.ty
  | CharacterLiteral _ -> Ctype.j_char_type
  | ArraySubscriptExpr a -> a.ty
  | BinaryOperator a -> a.ty
  | ConditionalOperator c -> exp_type c.then_expr
  | CXXBoolLiteralExpr _ -> Ctype.j_bool_type
  | CXXMethodDecl a -> a.ty
  | FloatingLiteral _ -> Ctype.j_float_type
  | FunctionDecl a -> a.ty
  | IntegerLiteral _ -> Ctype.j_int_type
  | NonTypeTemplateParmDecl a -> a.ty
  | ParmVarDecl a -> a.ty
  | UnaryOperator a -> a.ty
  | VarDecl a -> a.ty
  | CallExpr c -> c.ty
  | CXXOperatorCallExpr a -> a.ty
  | MemberExpr a -> a.ty
  | EnumConstantDecl a -> a.ty
  | UnresolvedLookupExpr a -> Ctype.mk_j_type "?"

let rec parse_position : json -> Sourceloc.position j_result =
  let open Sourceloc in
  let open Rjson in
  fun (j:json) ->
    let* o = cast_object j in
    match (
      let* line = with_field "line" cast_int o in
      let* col = with_field "col" cast_int o in
      let* filename = with_field_or "file" cast_string "" o in
      Ok {
        pos_line = line;
        pos_column = col;
        pos_filename = filename
      }
    ) with
    | Ok p -> Ok p
    | Error e -> with_field "expansionLoc" parse_position o
    

let parse_location (j:json) : Sourceloc.location j_result =
  let open Rjson in
  let open Sourceloc in
  let* o = cast_object j in
  let* s = with_field "begin" parse_position o in
  let* e = with_field "end" parse_position o in
  Ok {
    loc_start = s;
    loc_end = e;
  }

let parse_variable (j:json) : variable j_result =
  let open Rjson in
  let* o = cast_object j in
  let* name = with_field "name" cast_string o in
  match List.assoc_opt "range" o with
  | Some range ->
    let* l = with_field "range" parse_location o in
    Ok (LocVariable (l, name)) 
  | None -> Ok (Variable name)

let rec parse_exp (j:json) : c_exp j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "CharacterLiteral" ->
    let* i = with_field "value" cast_int o in
    Ok (CharacterLiteral i)

  | "ImplicitCastExpr"
  | "CXXStaticCastExpr"
  | "ConstantExpr"
  | "ParenExpr"
  | "ExprWithCleanups"
  | "CStyleCastExpr" ->
    with_field "inner" (cast_list_1 parse_exp) o

  | "CXXDependentScopeMemberExpr" ->
    let* n = with_field "member" cast_string o in
    let* b = with_field "inner" (fun i ->
      match cast_map parse_exp i with
      | Ok [o] -> Ok o
      | Ok l -> root_cause ("A list of length 1, but got " ^ (List.length l |> string_of_int)) i
      | Error e -> Error e
    ) o in
    let* ty = get_field "type" o in
    Ok (MemberExpr {name=n; base=b; ty=ty})

  | "DeclRefExpr" ->
    with_field "referencedDecl" parse_exp o

  | "FloatingLiteral" ->
    (match with_field "value" cast_int o with
        | Ok i -> Ok (FloatingLiteral (Float.of_int i))
        | _ ->
          let* f = with_field "value" cast_float o in
          Ok (FloatingLiteral f))

  | "IntegerLiteral" ->
    let* i = with_field "value" cast_string o in
    Ok (IntegerLiteral (int_of_string i))

  | "MemberExpr" ->
    let* n = with_field "name" cast_string o in
    let* b = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    Ok (MemberExpr {name=n; base=b; ty=ty})

  | "EnumConstantDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (EnumConstantDecl {name=v; ty=ty})

  | "VarDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (VarDecl {name=v; ty=ty})
    
  | "FunctionDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (FunctionDecl {name=v; ty=ty})

  | "CXXMethodDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (CXXMethodDecl {name=v; ty=ty})

  | "ConditionalOperator" ->
    let* (c, t, e) = with_field "inner"
      (cast_list_3 parse_exp parse_exp parse_exp) o
    in
    let* ty = get_field "type" o in
    Ok (ConditionalOperator {cond=c; then_expr=t; else_expr=e; ty=ty})

  | "ParmVarDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (ParmVarDecl {name=v; ty=ty})

  | "NonTypeTemplateParmDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (NonTypeTemplateParmDecl {name=v; ty=ty})

  | "UnresolvedLookupExpr" ->
    let* v = parse_variable j in
    let* tys = get_field "lookups" o >>= cast_list in
    Ok (UnresolvedLookupExpr {name=v; tys=tys})

  | "UnaryOperator" ->
    let* op = with_field "opcode" cast_string o in
    let* c = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    let inc o =
      BinaryOperator {ty=ty; opcode="="; lhs=c;
        rhs=BinaryOperator{ty=ty; opcode=o; lhs=c; rhs=IntegerLiteral 1}}
    in
    Ok (match op with
    | "++" -> inc "+"
    | "--" -> inc "-"
    | "+" -> c
    | "-" -> BinaryOperator {ty=ty; opcode=op; lhs=IntegerLiteral 0; rhs=c}
    | _ -> UnaryOperator {ty=ty; opcode=op; child=c})

  | "CompoundAssignOperator" ->
    (* Convert: x += e into x = x + y *)
    let* ty = get_field "computeResultType" o in
    let* lhs, rhs = with_field "inner" (cast_list_2 parse_exp parse_exp) o in
    let* opcode = with_field "opcode" cast_string o in
    (match Common.rsplit '=' opcode with
      | Some (opcode, "") ->
        Ok (BinaryOperator {
          ty=ty;
          opcode="=";
          lhs=lhs;
          rhs=BinaryOperator {ty=ty; opcode=opcode; lhs=lhs; rhs=rhs}
        })
      | _ -> root_cause "ERROR: parse_exp" j)

  | "BinaryOperator" ->
    let ty = List.assoc_opt "type" o |> Ojson.unwrap_or Ctype.j_int_type in
    let* opcode = with_field "opcode" cast_string o in
    let* lhs, rhs = with_field "inner"
      (cast_list_2 parse_exp parse_exp) o
    in
    Ok (BinaryOperator {ty=ty; opcode=opcode; lhs=lhs; rhs=rhs})

  | "ArraySubscriptExpr" ->
    let* ty = get_field "type" o in
    let* lhs, rhs = with_field "inner"
      (cast_list_2 parse_exp parse_exp) o
    in
    Ok (ArraySubscriptExpr {ty=ty; lhs=lhs; rhs=rhs})

  | "CXXOperatorCallExpr" ->
    let* (func, args) = with_field "inner" (fun j ->
      let* h, t = cast_cons j in
      let* func = wrap parse_exp (fun _ -> "func", j) h in
      let* args = wrap (map parse_exp) (fun _ -> "args", j) t in
      Ok (func, args)
    ) o in
    let* ty = get_field "type" o in
    Ok (CXXOperatorCallExpr {func=func; args=args; ty=ty})

  | "CallExpr" ->
    let* (func, args) = with_field "inner" (fun j ->
      let* h, t = cast_cons j in
      let* func = wrap parse_exp (fun _ -> "func", j) h in
      let* args = wrap (map parse_exp) (fun _ -> "args", j) t in
      Ok (func, args)
    ) o in
    let* ty = get_field "type" o in
    Ok (CallExpr {func=func; args=args; ty=ty})

  | "CXXFunctionalCastExpr"
  | "MaterializeTemporaryExpr" ->
    let* body = with_field "inner" (cast_list_1 parse_exp) o in
    Ok body

  | "CXXConstructExpr" ->
    let* ty = get_field "type" o in
    let* args = with_field_or "inner" (cast_map parse_exp) [] o in
    Ok (CXXConstructExpr {args=args; ty=ty})


  | "CXXBoolLiteralExpr" ->
    let* b = with_field "value" cast_bool o in
    Ok (CXXBoolLiteralExpr b)

  | _ ->
    root_cause "ERROR: parse_exp" j

let rec parse_init (j:json) : c_init j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
    
  | "InitListExpr" ->
    let* ty = get_field "type" o in
    let* args = with_field "inner" (cast_map parse_exp) o in
    Ok (InitListExpr {ty=ty; args=args})

  | _ ->
    let* e = parse_exp j in
    Ok (IExp e)

let parse_attr (j:Yojson.Basic.t) : string j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  with_field "value" cast_string o

let is_valid_j : json -> bool =
  function
  | `Assoc o -> List.assoc_opt "kind" o |> Option.is_some
  | _ -> false

let parse_decl (j:json) : c_decl j_result =
  let open Rjson in
  let* v = parse_variable j in
  let* o = cast_object j in
  let* ty = get_field "type" o in
  let* kind = get_kind o in
  let inner = List.assoc_opt "inner" o |> Ojson.unwrap_or (`List []) in
  let* inner = cast_list inner in
  let inner = List.filter is_valid_j inner in
  let attrs, inits = List.partition (fun j ->
    (
      let* o = cast_object j in
      let* k = get_kind o in
      Ok (match k with 
        | "AnnotateAttr" -> true
        | _ -> false
      )
    ) |> unwrap_or false
  ) inner in
  let* attrs = map parse_attr attrs in
  let* inits = map parse_init inits in
  (* Further enforce that there is _at most_ one init expression. *)
  let* init = match inits with
  | [init] -> Ok (Some init)
  | [] -> Ok None
  | _ ->
    (* Print out a nice error message with provenance. *)
    let i = List.length inits |> string_of_int in
    let msg = "Expecting at most one expression, but got " ^ i in
    let open StackTrace in
    Error (Because (("Field 'init'", j), RootCause (msg, `List inner)))
  in
  Ok {name=v; ty=ty; init=init; attrs=attrs}

let parse_for_init (j:json) : c_for_init j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "DeclStmt" ->
    let* ds = with_field "inner" (cast_map parse_decl) o in
    Ok (ForDecl ds)
  | _ ->
    let* e = parse_exp j in
    Ok (ForExp e)

let rec parse_stmt (j:json) : c_stmt j_result =
  let open Rjson in
  let* o = cast_object j in
  match get_kind o |> Result.to_option with
  | Some "IfStmt" ->
    let* (cond, then_stmt, else_stmt) = with_field "inner" (fun j ->
      let* l = cast_list j in
      let wrap handle_ok (m:string) = wrap handle_ok (fun _ -> (m, j)) in
      match l with
      | [cond;then_stmt;else_stmt] ->
        let* cond = wrap parse_exp "cond" cond in
        let* then_stmt = wrap parse_stmt "then_stmt" then_stmt in
        let* else_stmt = wrap parse_stmt "else_stmt" else_stmt in
        Ok (cond, then_stmt, else_stmt)
      | [cond;then_stmt] ->
        let* cond = wrap parse_exp "cond" cond in
        let* then_stmt = wrap parse_stmt "then_stmt" then_stmt in
        Ok (cond, then_stmt, CompoundStmt [])
      | _ ->
        let g = List.length l |> string_of_int in
        root_cause ("Expecting a list of length 2 or 3, but got a length of list " ^ g) j
    ) o
    in
    Ok (IfStmt {cond=cond; then_stmt=then_stmt; else_stmt=else_stmt})
  | Some "WhileStmt" ->
    let* (cond, body) = with_field "inner"
      (cast_list_2 parse_exp parse_stmt) o
    in
    Ok (WhileStmt {cond=cond; body=body})
  | Some "DeclStmt" ->
    let* children = with_field "inner" (cast_map parse_decl) o in
    Ok (DeclStmt children)
  | Some "DefaultStmt" ->
    let* c = with_field "inner" (cast_list_1 parse_stmt) o in
    Ok (DefaultStmt c)
  | Some "CaseStmt" ->
    let* (c, b) = with_field "inner"
      (cast_list_2 parse_exp parse_stmt) o
    in
    Ok (CaseStmt {case=c; body=b})
  | Some "SwitchStmt" ->
    let* (c, b) = with_field "inner"
      (cast_list_2 parse_exp parse_stmt) o
    in
    Ok (SwitchStmt {cond=c; body=b})
  | Some "CompoundStmt" ->
    let* children : c_stmt list = with_field_or "inner" (fun (i:json) ->
      match i with
      | `Assoc _ -> let* o = parse_stmt i in Ok [o]
      | _ -> parse_stmt_list i
    ) [] o in
    Ok (CompoundStmt children)
  | Some "LabelStmt" ->
    (* TODO: do not parse LabelStmt *) 
    with_field "inner" (cast_list_1 parse_stmt) o
  | Some "ReturnStmt" ->
    Ok ReturnStmt
  | Some "GotoStmt" ->
    Ok GotoStmt
  | Some "BreakStmt" ->
    Ok BreakStmt
  | Some "DoStmt" ->
    let* inner = with_field "inner" cast_list o in
    let* b, c = match inner with
    | [b; c] ->
      let* b = parse_stmt b in
      let* c = parse_exp c in
      Ok (b, c)
    | [b] ->
      let* b = parse_stmt b in
      Ok (b, CXXBoolLiteralExpr true)
    | _ -> root_cause "Error parsing DoStmt" j
    in
    Ok (DoStmt {cond=c; body=b})
  | Some "AttributedStmt" ->
    let* (_, stmt) = with_field "inner" (cast_list_2 Result.ok parse_stmt) o in
    Ok stmt
  | Some "ForStmt" ->
    let* (init, cond, inc, body) = with_field "inner" (fun j ->
      let* l = cast_list j in
      let wrap handle_ok (m:string) = wrap handle_ok (fun _ -> (m, j)) in
      let wrap_opt handle_ok (m:string) (j:Yojson.Basic.t) =
        match j with
        | `Assoc [] -> Ok None
        | _ ->
          let* r = wrap handle_ok m j in
          Ok (Some r)
      in
      match l with
      | [init; _; cond; inc; body] ->
        let* init = wrap_opt parse_for_init "init" init in
        let* cond = wrap_opt parse_exp "cond" cond in
        let* inc = wrap_opt parse_exp "inc" inc in
        let* body = wrap parse_stmt "body" body in
        Ok (init, cond, inc, body)
      | _ ->
        let g = List.length l |> string_of_int in
        root_cause ("Expecting a list of length 5, but got a length of list " ^ g) j
    ) o in
    Ok (ForStmt {init=init; cond=cond; inc=inc; body=body})
  | Some "NullStmt" -> Ok (CompoundStmt [])
  | Some _ ->
    let* e = parse_exp j in
    Ok (SExp e)
  | None -> Ok (CompoundStmt [])

and parse_stmt_list = fun inner ->
  let open Rjson in
  cast_list inner
  >>= map_all parse_stmt
    (fun idx s e -> StackTrace.Because (("error parsing statement #" ^ string_of_int (idx + 1), s), e))

let parse_param (j:json) : c_param j_result =
  let open Rjson in
  let* v = parse_variable j in
  let* o = cast_object j in
  let* ty = get_field "type" o in
  let* is_refed = with_field_or "isReferenced" cast_bool false o in
  let* is_used =  with_field_or "isUsed" cast_bool false o in
  let* is_shared = with_field_or "shared" cast_bool false o in
  Ok {name=v; is_used=(is_refed || is_used); is_shared=is_shared; ty=ty}

let c_attr (k:string) : string =
  " __attribute__((annotate(\"" ^ k ^ "\")))"

let c_attr_shared = c_attr "shared"
let c_attr_global = c_attr "global"

let j_filter_kind (f:string -> bool) (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let res =
    let* o = cast_object j in
    let* k = get_kind o in
    Ok (f k)
  in
  res |> unwrap_or false

let parse_kernel (j:Yojson.Basic.t) : c_kernel j_result =
  let open Rjson in
  let* o = cast_object j in
  let* inner = with_field "inner" cast_list o in
  let attrs, inner =
    inner
    |> List.partition
      (j_filter_kind (Common.ends_with "Attr"))
  in
  let ps, body =
    inner
    |> List.partition
      (j_filter_kind (fun k -> k = "ParmVarDecl" || k = "TemplateArgument"))
  in
  let* attrs = map parse_attr attrs in
  let* body: c_stmt list = parse_stmt_list (Yojson.Basic.(`List body)) in
  let body = match body with
    | [s] -> s
    | _ -> CompoundStmt body
  in
  let* name: string = with_field "name" cast_string o in
  let* ps = map parse_param ps in
  Ok {
    name = name;
    code = body;
    params = ps;
  }

let is_kernel2 (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let is_kernel =
    let* o = cast_object j in
    let* k = get_kind o in
    if k = "FunctionDecl" then
      let* inner = with_field "inner" cast_list o in
      (* Try to parse attrs *)
      let attrs = inner
        |> Common.map_opt (fun j ->
          parse_attr j >>= (fun a -> Ok (Some a))
          |> unwrap_or None
        )
      in
      Ok (List.mem c_attr_global attrs)
    else Ok false
  in
  is_kernel |> unwrap_or false

let is_kernel (j:Yojson.Basic.t) : bool =
  let open Rjson in
  cast_object j
  >>= with_field "is_kernel" cast_bool
  |> unwrap_or false


let parse_kernels (j:Yojson.Basic.t) : c_kernel list j_result =
  let open Rjson in
  cast_object j
    >>= with_field "inner" cast_list
    |> unwrap_or [] (* ignore errors and convert it to an empty list *)
    |> List.filter is_kernel2 (* only keep things that look like kernels *)
    |> map_all (* for each kernel convert it into an object and parse it *)
      parse_kernel
      (* Abort the whole thing if we find a single parsing error *)
      (fun idx k e -> StackTrace.Because (("error parsing kernel " ^ string_of_int idx, k), e))

let parse_type (j:Yojson.Basic.t) : Ctype.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* ty = with_field "qualType" cast_string o in
  Ok (Ctype.make ty)

let type_to_str (j:Yojson.Basic.t) : string =
  match parse_type j with
  | Ok ty -> Ctype.to_string ty
  | Error _ -> "?"

(* ------------------------------------------------------------------------ *)

let list_to_s (f:'a -> string) (l:'a list) : string =
  List.map f l |> Common.join ", "

let rec exp_to_s : c_exp -> string =
  function
  | FloatingLiteral f -> string_of_float f
  | CharacterLiteral i
  | IntegerLiteral i -> string_of_int i
  | ConditionalOperator c ->
    "(" ^ exp_to_s c.cond ^ ") ? (" ^
          exp_to_s c.then_expr ^ ") : (" ^
          exp_to_s c.else_expr ^ ")"
  | BinaryOperator b -> "(" ^ exp_to_s b.lhs ^ ") (" ^ b.opcode ^ "." ^ type_to_str b.ty ^ ") (" ^ exp_to_s b.rhs ^ ")"
  | MemberExpr m -> "("^ exp_to_s m.base  ^ ")." ^ m.name
  | ArraySubscriptExpr b -> exp_to_s b.lhs ^ "[" ^ exp_to_s b.rhs ^ "]"
  | CXXBoolLiteralExpr b -> if b then "true" else "false";
  | CXXConstructExpr c -> "@ctor " ^ type_to_str c.ty ^ "(" ^ list_to_s exp_to_s c.args ^ ")" 
  | CXXOperatorCallExpr c -> exp_to_s c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
  | CXXMethodDecl v -> "@meth " ^ var_name v.name
  | CallExpr c -> exp_to_s c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
  | VarDecl v -> var_name v.name
  | UnresolvedLookupExpr v -> "@unresolv " ^ var_name v.name
  | NonTypeTemplateParmDecl v -> "@tpl " ^ var_name v.name
  | FunctionDecl v -> "@func " ^ var_name v.name
  | ParmVarDecl v -> "@parm " ^ var_name v.name
  | EnumConstantDecl v -> "@enum " ^ var_name v.name
  | UnaryOperator u -> u.opcode ^ exp_to_s u.child

let init_to_s : c_init -> string =
  function
  | InitListExpr i -> list_to_s exp_to_s i.args
  | IExp i -> exp_to_s i

let decl_to_s (d: c_decl): string =
  let i = match d.init with
    | Some e -> " = " ^ init_to_s e
    | None -> ""
  in
  var_name d.name ^ i


let for_init_to_s (f:c_for_init) : string =
  match f with
  | ForDecl d -> list_to_s decl_to_s d
  | ForExp e -> exp_to_s e

let opt_for_init_to_s (o:c_for_init option) : string =
  match o with
  | Some o -> for_init_to_s o
  | None -> ""

let stmt_to_s: c_stmt -> PPrint.t list =
  let opt_exp_to_s: c_exp option -> string =
    function
    | Some c -> exp_to_s c
    | None -> ""
  in
  let open PPrint in
  let rec stmt_to_s : c_stmt -> PPrint.t list =
    function
    | ReturnStmt -> [Line "return;"]
    | GotoStmt -> [Line "goto;"]
    | BreakStmt -> [Line "break;"]
    | ForStmt f -> [
        Line ("for " ^ opt_for_init_to_s f.init ^ "; " ^ opt_exp_to_s f.cond ^ "; " ^ opt_exp_to_s f.inc ^ ") {");
        Block(stmt_to_s f.body);
        Line ("}")
      ]
    | WhileStmt {cond=b; body=s} -> [
        Line ("while (" ^ exp_to_s b ^ ") {");
        Block (stmt_to_s s);
        Line "}"
      ]
    | DoStmt {cond=b; body=s} -> [
        Line "}";
        Block (stmt_to_s s);
        Line ("do (" ^ exp_to_s b ^ ") {");
      ]
    | SwitchStmt {cond=b; body=s} -> [
        Line ("switch " ^ exp_to_s b ^ " {");
        Block (stmt_to_s s);
        Line ("}");
      ]
    | CaseStmt c ->
      [ Line ("case " ^ exp_to_s c.case ^ ":"); Block(stmt_to_s c.body) ]
    | DefaultStmt d ->
      [ Line ("default:"); Block(stmt_to_s d) ]
    | IfStmt {cond=b; then_stmt=s1; else_stmt=s2} -> [
        Line ("if (" ^ exp_to_s b ^ ") {");
        Block (stmt_to_s s1);
        Line "} else {";
        Block (stmt_to_s s2);
        Line "}"
      ]
    | CompoundStmt l -> [Line "{"; Block (List.concat_map stmt_to_s l); Line "}"]
    | DeclStmt d -> [Line "decl {"; Block (List.map (fun e -> Line (decl_to_s e)) d); Line "}"]
    | SExp e -> [Line (exp_to_s e)]
  in
  stmt_to_s

let param_to_s (p:c_param) : string =
  let used = if p.is_used then "" else " unsed" in
  let shared = if p.is_shared then "shared " else "" in
  used ^ shared ^ var_name p.name

let kernel_to_s (k:c_kernel) : PPrint.t list =
  let open PPrint in
  [
    Line ("name: " ^ k.name);
    Line ("params: " ^ list_to_s param_to_s k.params);
  ]
  @
  stmt_to_s k.code

let print_kernel (k: c_kernel) : unit =
  PPrint.print_doc (kernel_to_s k)


