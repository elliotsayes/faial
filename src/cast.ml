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
  | CallExpr of {func: c_exp; args: c_exp list}
  | ConditionalOperator of {cond: c_exp; then_expr: c_exp; else_expr: c_exp; ty: c_type}
  | CXXBoolLiteralExpr of bool
  | CXXMethodDecl of {name: variable; ty: c_type}
  | CXXOperatorCallExpr of {func: c_exp; args: c_exp list}
  | FloatingLiteral of float
  | FunctionDecl of {name: variable; ty: c_type}
  | IntegerLiteral of int
  | NonTypeTemplateParmDecl of {name: variable; ty: c_type}
  | MemberExpr of {name: string; base: c_exp}
  | ParmVarDecl of {name: variable; ty: c_type}
  | DeclRefExpr of c_type
  | PredicateExpr of {child: c_exp; opcode: string}
  | UnaryOperator of {opcode: string; child: c_exp; ty: c_type}
  | VarDecl of {name: variable; ty: c_type}
  | UnresolvedLookupExpr of {name: variable; tys: c_type list}
and c_binary = {opcode: string; lhs: c_exp; rhs: c_exp; ty: c_type}
and c_array_subscript = {lhs: c_exp; rhs: c_exp; ty: c_type}

let exp_name = function
| CharacterLiteral _ -> "CharacterLiteral"
| ArraySubscriptExpr _ -> "ArraySubscriptExpr"
| BinaryOperator _ -> "BinaryOperator"
| CallExpr _ -> "CallExpr"
| ConditionalOperator _ -> "ConditionalOperator"
| CXXBoolLiteralExpr _ -> "CXXBoolLiteralExpr"
| CXXMethodDecl _ -> "CXXMethodDecl"
| CXXOperatorCallExpr _ -> "CXXOperatorCallExpr"
| FloatingLiteral _ -> "FloatingLiteral"
| FunctionDecl _ -> "FunctionDecl"
| IntegerLiteral _ -> "IntegerLiteral"
| NonTypeTemplateParmDecl _ -> "NonTypeTemplateParmDecl"
| MemberExpr _ -> "MemberExpr"
| ParmVarDecl _ -> "ParmVarDecl"
| DeclRefExpr _ -> "DeclRefExpr"
| PredicateExpr _ -> "PredicateExpr"
| UnaryOperator _ -> "UnaryOperator"
| VarDecl _ -> "VarDecl"
| UnresolvedLookupExpr _ -> "UnresolvedLookupExpr"


type c_init =
  | CXXConstructExpr of {constructor: c_type; ty: c_type}
  | InitListExpr of {ty: c_type; args: c_exp list}
  | IExp of c_exp
  

type c_range = {
  name: variable;
  lower_bound: c_exp;
  upper_bound: c_exp;
  step: c_exp;
  opcode: string
}

type c_decl = {
  name: variable;
  ty: c_type;
  init: c_init option;
  attrs: string list
}

type c_stmt =
  | BreakStmt
  | GotoStmt
  | ReturnStmt
  | IfStmt of {cond: c_exp; then_stmt: c_stmt; else_stmt: c_stmt}
  | CompoundStmt of c_stmt list
  | DeclStmt of c_decl list
  | WhileStmt of {cond: c_exp; body: c_stmt}
  | ForStmt of {init: c_exp option; cond: c_exp option; inc: c_exp option; body: c_stmt}
  | DoStmt of {cond: c_exp; body: c_stmt}
  | SwitchStmt of {cond: c_exp; body: c_stmt}
  | DefaultStmt of c_stmt
  | CaseStmt of {case: c_exp; body: c_stmt}
  | SyncStmt (* faial-infer *)
  | ForEachStmt of {range: c_range; body: c_stmt} (* faial-infer *)
  | AssertStmt of c_exp (* faial-infer *)
  | SExp of c_exp

type c_kernel = {
  name: string;
  code: c_stmt;
}


let mk_type name =
  let open Yojson in
  `Assoc[
    "qualType", `String name
  ]

let int_type = mk_type "int"
let char_type = mk_type "char"
let bool_type = mk_type "bool"
let float_type = mk_type "float"

let rec exp_type (e:c_exp) : c_type =
  match e with
  | CharacterLiteral _ -> char_type
  | ArraySubscriptExpr a -> a.ty
  | BinaryOperator a -> a.ty
  | ConditionalOperator c -> exp_type c.then_expr
  | CXXBoolLiteralExpr _ -> bool_type
  | CXXMethodDecl a -> a.ty
  | FloatingLiteral _ -> float_type
  | FunctionDecl a -> a.ty
  | IntegerLiteral _ -> int_type
  | NonTypeTemplateParmDecl a -> a.ty
  | ParmVarDecl a -> a.ty
  | DeclRefExpr ty -> ty
  | PredicateExpr a -> bool_type
  | UnaryOperator a -> a.ty
  | VarDecl a -> a.ty
  (* ----- *)
  | CallExpr c -> mk_type "?"
  | CXXOperatorCallExpr a -> mk_type "?"
  | MemberExpr a -> mk_type "?"
  | UnresolvedLookupExpr a -> mk_type "?"

let parse_mode (j:json) : mode j_result =
  let open Rjson in
  let* m = cast_string j in
  match m with
  | "ro" -> Ok R
  | "rw" -> Ok W
  | e -> root_cause ("parse_mode: expecting either 'ro' or 'rw', but got '" ^ e ^ "'") j

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
  | "ConstantExpr" ->
    with_field "inner" (fun f ->
      let* l = cast_list f >>= ensure_length_eq 1 in
      with_index 0 parse_exp l
    ) o

  | "CharacterLiteral" ->
    let* i = with_field "value" cast_int o in
    Ok (CharacterLiteral i)

  | "IntegerLiteral" ->
    let* i = with_field "value" cast_int o in
    Ok (IntegerLiteral i)

  | "FloatingLiteral" ->
    (match with_field "value" cast_int o with
    | Ok i -> Ok (FloatingLiteral (Float.of_int i))
    | _ ->
      let* f = with_field "value" cast_float o in
      Ok (FloatingLiteral f))

  | "DeclRefExpr" ->
    let* ty = get_field "type" o in
    Ok (DeclRefExpr ty)

  | "CXXDependentScopeMemberExpr" ->
    let* n = with_field "member" cast_string o in
    let* b = with_field "inner" (fun i ->
      match cast_map parse_exp i with
      | Ok [o] -> Ok o
      | Ok l -> root_cause ("A list of length 1, but got " ^ (List.length l |> string_of_int)) i
      | Error e -> Error e
    ) o in
    Ok (MemberExpr {name=n; base=b})

  | "MemberExpr" ->
    let* n = with_field "name" cast_string o in
    let* b = with_field "base" parse_exp o in
    Ok (MemberExpr {name=n; base=b})

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
    let* c = with_field "cond" parse_exp o in
    let* t = with_field "thenExpr" parse_exp o in
    let* e = with_field "elseExpr" parse_exp o in
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

  | "PredicateExpr" ->
    let* op = with_field "opcode" cast_string o in
    let* c = with_field "subExpr" parse_exp o in
    Ok (PredicateExpr {opcode=op; child=c})

  | "UnaryOperator" ->
    let* op = with_field "opcode" cast_string o in
    let* c = with_field "subExpr" parse_exp o in
    let* ty = get_field "type" o in
    Ok (UnaryOperator {ty=ty; opcode=op; child=c})

  | "BinaryOperator" ->
    let ty = List.assoc_opt "type" o |> Ojson.unwrap_or int_type in
    let* opcode = with_field "opcode" cast_string o in
    let* lhs = with_field "lhs" parse_exp o in
    let* rhs = with_field "rhs" parse_exp o in
    Ok (BinaryOperator {ty=ty; opcode=opcode; lhs=lhs; rhs=rhs})

  | "ArraySubscriptExpr" ->
    let* ty = get_field "type" o in
    let* lhs = with_field "lhs" parse_exp o in
    let* rhs = with_field "rhs" parse_exp o in
    Ok (ArraySubscriptExpr {ty=ty; lhs=lhs; rhs=rhs})

  | "CXXOperatorCallExpr" ->
    let* func = with_field "func" parse_exp o in
    let* args = with_field "args" (cast_map parse_exp) o in
    Ok (CXXOperatorCallExpr {func=func; args=args})

  | "CallExpr" ->
    let* func = with_field "func" parse_exp o in
    let* args = with_field "args" (cast_map parse_exp) o in
    Ok (CallExpr {func=func; args=args})

  | "CXXFunctionalCastExpr"
  | "MaterializeTemporaryExpr" ->
    let* body = with_field "inner" (fun b ->
      cast_list b >>= ensure_length_eq 1 >>=
      with_index 0 parse_exp
    ) o in
    Ok body
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

  | "CXXConstructExpr" ->
    let* ty = get_field "type" o in
    let* ctor = get_field "ctorType" o in
    Ok (CXXConstructExpr {constructor=ctor; ty=ty})

  | _ ->
    let* e = parse_exp j in
    Ok (IExp e)

let parse_decl (j:json) : c_decl j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "VarDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    let* init, attrs = match List.assoc_opt "inner" o with
    | None -> Ok (None, [])
    | Some orig -> begin
      let* inits, attrs = cast_map (fun j ->
        (* For each element of the init list, we test if its an annotation
           or the value being initialized. There may exist many annotations,
           but only one expression. *)
        let* o = cast_object j in
        let* kind = get_kind o in
        let open Either in
        match kind with
        | "AnnotateAttr" ->
          let* i = with_field "value" cast_string o in
          (* Tag an annotation *)
          Ok (Right i)
        | _ ->
          (* Tag an init *)
          let* e = parse_init j in
          Ok (Left e)
       ) orig
       (* Partition the list into two according to the tag *)
       |> Result.map (List.partition_map (fun x -> x))
      in
      (* Further enforce that there is _at most_ one init expression. *)
      let* init = match inits with
      | [init] -> Ok (Some init)
      | [] -> Ok None
      | _ ->
        (* Print out a nice error message with provenance. *)
        let i = List.length inits |> string_of_int in
        let msg = "Expecting at most one expression, but got " ^ i in
        let open StackTrace in
        Error (Because (("Field 'init'", j), RootCause (msg, orig)))
      in
      Ok (init, attrs)
      end
    in
    Ok {name=v; ty=ty; init=init; attrs=attrs}
  | _ -> 
    root_cause ("ERROR: parse_decl") j


let parse_range (v:variable) (j:json) : c_range j_result =
  let open Rjson in
  let* o = cast_object j in
  let* lb = with_field "init" parse_exp o in
  let* upper_bound = with_field "upper_bound" parse_exp o in
  let* step = with_field "step" parse_exp o in
  let* opcode = with_field "opcode" cast_string o in
  Ok {
    name=v;
    lower_bound=lb;
    upper_bound=upper_bound;
    step=step;
    opcode=opcode
  }

let rec parse_stmt (j:json) : c_stmt j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "SyncStmt" -> Ok SyncStmt
  | "AssertStmt" ->
    let* cond = with_field "cond" parse_exp o in
    Ok (AssertStmt cond)
  | "IfStmt" ->
    let* cond = with_field "cond" parse_exp o in
    let* then_stmt = with_field "thenStmt" parse_stmt o in
    let* else_stmt = with_field_or "elseStmt" parse_stmt (CompoundStmt []) o in
    Ok (IfStmt {cond=cond; then_stmt=then_stmt; else_stmt=else_stmt})
  | "WhileStmt" ->
    let* cond = with_field "cond" parse_exp o in
    let* body = with_field "body" parse_stmt o in
    Ok (WhileStmt {cond=cond; body=body})
  | "DeclStmt" ->
    (* prerr_endline (Yojson.Basic.pretty_to_string j); *)
    let* children = with_field "inner" (cast_map parse_decl) o in
    Ok (DeclStmt children)
  | "DefaultStmt" ->
    let* c = with_field "inner" (fun i ->
      cast_list i >>= ensure_length_eq 1 >>= with_index 0 parse_stmt 
    ) o in
    Ok (DefaultStmt c)
  | "CaseStmt" ->
    let* (c, b) = with_field "inner" (fun f ->
      let* l = cast_list f >>= ensure_length_eq 2 in
      let* c = with_index 0 parse_exp l in
      let* b = with_index 1 parse_stmt l in
      Ok (c, b)
    ) o in
    Ok (CaseStmt {case=c; body=b})
  | "SwitchStmt" ->
    let* (c, b) = with_field "inner" (fun f ->
      let* l = cast_list f >>= ensure_length_eq 2 in
      let* c = with_index 0 parse_exp l in
      let* b = with_index 1 parse_stmt l in
      Ok (c, b)
    ) o in
    Ok (SwitchStmt {cond=c; body=b})
  | "CompoundStmt" ->
    let* children : c_stmt list = with_field "inner" (fun (i:json) ->
      match i with
      | `Assoc _ -> let* o = parse_stmt i in Ok [o]
      | _ -> parse_stmt_list i
    ) o in
    Ok (CompoundStmt children)
  | "ReturnStmt" ->
    Ok ReturnStmt
  | "GotoStmt" ->
    Ok GotoStmt
  | "BreakStmt" ->
    Ok BreakStmt
  | "DoStmt" ->
    let* c = with_field_or "cond" parse_exp (CXXBoolLiteralExpr true) o in
    let* b = with_field "body" parse_stmt o in
    Ok (DoStmt {cond=c; body=b})
  | "ForStmt" ->
    let* i = with_opt_field "init" parse_exp o in
    let* c = with_opt_field "cond" parse_exp o in
    let* n = with_opt_field "inc" parse_exp o in
    let* b = with_field "body" parse_stmt o in
    Ok (ForStmt {init=i; cond=c; inc=i; body=b})
  | "ForEachStmt" ->
    let* v = with_field "var" parse_variable o in
    let* r = with_field "range" (parse_range v) o in
    let* b = with_field "body" parse_stmt o in
    Ok (ForEachStmt {range=r; body=b})
  | _ ->
    let* e = parse_exp j in
    Ok (SExp e)

and parse_stmt_list = fun inner ->
  let open Rjson in
  cast_list inner
  >>= map_all parse_stmt
    (fun idx s e -> StackTrace.Because (("error parsing statement #" ^ string_of_int (idx + 1), s), e))

let parse_kernel (o:Rjson.j_object) : c_kernel j_result =
  let open Rjson in
  let* body: c_stmt = with_field "body" parse_stmt o in
  let* name: string = with_field "name" cast_string o in
  Ok {
    name = name;
    code = body;
  }

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
    |> List.filter is_kernel (* only keep things that look like kernels *)
    |> map_all (* for each kernel convert it into an object and parse it *)
      (fun k -> cast_object k >>= parse_kernel)
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
  | CXXOperatorCallExpr c -> exp_to_s c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
  | CXXBoolLiteralExpr b -> if b then "true" else "false";
  | CallExpr c -> exp_to_s c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
  | VarDecl v -> var_name v.name
  | DeclRefExpr t -> Yojson.Basic.pretty_to_string t
  | UnresolvedLookupExpr v -> "@unresolv " ^ var_name v.name
  | NonTypeTemplateParmDecl v -> "@tpl " ^ var_name v.name
  | CXXMethodDecl v -> "@meth " ^ var_name v.name
  | FunctionDecl v -> "@func " ^ var_name v.name
  | ParmVarDecl v -> "@parm " ^ var_name v.name
  | PredicateExpr p -> p.opcode ^ "(" ^ exp_to_s p.child ^ ")"
  | UnaryOperator u -> u.opcode ^ exp_to_s u.child

let init_to_s : c_init -> string =
  function
  | CXXConstructExpr c -> "ctor"
  | InitListExpr i -> list_to_s exp_to_s i.args
  | IExp i -> exp_to_s i

let decl_to_s (d: c_decl): string =
  let i = match d.init with
    | Some e -> " = " ^ init_to_s e
    | None -> ""
  in
  var_name d.name ^ i


let range_to_s (r:c_range) : string =
  exp_to_s r.lower_bound ^ " .. " ^ exp_to_s r.upper_bound ^ "; " ^ r.opcode ^ exp_to_s r.step

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
    | SyncStmt -> [Line "sync;"]
    | AssertStmt b -> [Line ("assert (" ^ (exp_to_s b) ^ ");")]
    | ForStmt f -> [
        Line ("for " ^ opt_exp_to_s f.init ^ "; " ^ opt_exp_to_s f.cond ^ "; " ^ opt_exp_to_s f.inc ^ ") {");
        Block(stmt_to_s f.body);
        Line ("}")
      ]
    | ForEachStmt {range=r; body=b} ->
      [ Line ("foreach " ^ (var_name r.name) ^ " in " ^ range_to_s r ^ " {");
        Block (stmt_to_s b); Line "}"]
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

let kernel_to_s (k:c_kernel) : PPrint.t list =
  let open PPrint in
  stmt_to_s k.code

let print_kernel (k: c_kernel) : unit =
  PPrint.print_doc (kernel_to_s k)


