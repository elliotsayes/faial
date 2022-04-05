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
  | ArraySubscriptExpr of {lhs: c_exp; rhs: c_exp; ty: c_type}
  | BinaryOperator of {opcode: string; lhs: c_exp; rhs: c_exp; ty: c_type}
  | CallExpr of {func: c_exp; args: c_exp list}
  | ConditionalOperator of {cond: c_exp; then_expr: c_exp; else_expr: c_exp; ty: c_type}
  | CXXBoolLiteralExpr of bool
  | CXXConstructExpr of {constructor: c_type; ty: c_type} (* Only in decl *)
  | CXXMethodDecl of {name: variable; ty: c_type}
  | CXXOperatorCallExpr of {func: c_exp; args: c_exp list}
  | FloatingLiteral of float
  | FunctionDecl of {name: variable; ty: c_type}
  | IntegerLiteral of int
  | InitListExpr of {ty: c_type; args: c_exp list} (* Only in decl *)
  | NonTypeTemplateParmDecl of {name: variable; ty: c_type}
  | MemberExpr of {name: string; base: c_exp}
  | ParmVarDecl of {name: variable; ty: c_type}
  | UnaryOperator of { opcode: string; child: c_exp; ty: c_type}
  | VarDecl of {name: variable; ty: c_type; init: c_exp option (* Only in decl *)}
  | UnresolvedLookupExpr of {name: variable; tys: c_type list}
  | Unknown of json

type c_range = {init: c_exp; upper_bound: c_exp; step: c_exp; opcode: string}

type c_stmt =
  | BreakStmt
  | IfStmt of {cond: c_exp; then_stmt: c_stmt; else_stmt: c_stmt}
  | CompoundStmt of c_stmt list
  | DeclStmt of c_exp list
  | WhileStmt of {cond: c_exp; body: c_stmt}
  | SyncStmt (* faial-infer *)
  | ForEachStmt of {var: variable; range: c_range; body: c_stmt} (* faial-infer *)
  | AccessStmt of {location: c_exp; mode: mode; index: c_exp list } (* faial-infer *)
  | AssertStmt of c_exp (* faial-infer *)
  | LocationAliasStmt of {source: c_exp; target: c_exp; offset: c_exp} (* faial-infer *)
  | CExp of c_exp

type c_kernel = {
  name: string;
  code: c_stmt;
}

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
  let parse_exp_list = fun f ->
      cast_list f
      >>= map_all
        parse_exp
        (fun idx s e -> Because ("error parsing expression #" ^ (string_of_int (idx + 1)), s, e))
  in
  match kind with
  | "IntegerLiteral" ->
    let* i = with_field "value" cast_int o in
    Ok (IntegerLiteral i)

  | "FloatingLiteral" ->
    (match with_field "value" cast_int o with
    | Ok i -> Ok (FloatingLiteral (Float.of_int i))
    | _ ->
      let* f = with_field "value" cast_float o in
      Ok (FloatingLiteral f))

  | "InitListExpr" ->
    let* ty = get_field "type" o in
    let* args = with_field "inner" parse_exp_list o in
    Ok (InitListExpr {ty=ty; args=args})

  | "CXXDependentScopeMemberExpr" ->
    let* n = with_field "member" cast_string o in
    let* b = with_field "inner" (fun i ->
      match parse_exp_list i with
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
    let* init = match List.assoc_opt "inner" o with
    | Some (`List [e]) ->
      let* e = parse_exp e in
      Ok (Some e)
    | _ -> Ok None
    in
    Ok (VarDecl {name=v; ty=ty; init=init})
    
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

  | "UnaryOperator" ->
    let* op = with_field "opcode" cast_string o in
    let* c = with_field "subExpr" parse_exp o in
    let* ty = get_field "type" o in
    Ok (UnaryOperator {ty=ty; opcode=op; child=c})

  | "BinaryOperator" ->
    let* ty = get_field "type" o in
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
    let* args = with_field "args" parse_exp_list o in
    Ok (CXXOperatorCallExpr {func=func; args=args})

  | "CallExpr" ->
    let* func = with_field "func" parse_exp o in
    let* args = with_field "args" parse_exp_list o in
    Ok (CallExpr {func=func; args=args})

  | "CXXConstructExpr" ->
    let* ty = get_field "type" o in
    let* ctor = get_field "ctorType" o in
    Ok (CXXConstructExpr {constructor=ctor; ty=ty})

  | "CXXFunctionalCastExpr"
  | "MaterializeTemporaryExpr" ->
    let* body = with_field "inner" (fun b ->
      cast_list b >>=
      with_index 0 parse_exp
    ) o in
    Ok body

  | "CXXBoolLiteralExpr" ->
    let* b = with_field "value" cast_bool o in
    Ok (CXXBoolLiteralExpr b)

  | _ -> Ok (Unknown (`Assoc o))

let parse_range (j:json) : c_range j_result =
  let open Rjson in
  let* o = cast_object j in
  let* init = with_field "init" parse_exp o in
  let* upper_bound = with_field "upper_bound" parse_exp o in
  let* step = with_field "step" parse_exp o in
  let* opcode = with_field "opcode" cast_string o in
  Ok {init=init; upper_bound=upper_bound; step=step; opcode=opcode}

let rec parse_stmt (j:json) : c_stmt j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  let parse_exp_list = fun f ->
      cast_list f
      >>= map_all
        parse_exp
        (fun idx s e -> Because ("error parsing expression #" ^ (string_of_int (idx + 1)), s, e))
  in
  let parse_stmt_list = fun inner ->
      cast_list inner
      >>= map_all parse_stmt
        (fun idx s e -> Because ("error parsing statement #" ^ (string_of_int (idx + 1)), s, e))
  in
  match kind with
  | "SyncStmt" -> Ok SyncStmt
  | "AccessStmt" ->
    let* loc = with_field "location" parse_exp o in
    let* mode = with_field "mode" parse_mode o in
    let* index = with_field "index" parse_exp_list o in
    Ok (AccessStmt {location=loc; mode=R; index=index})
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
    let* children = with_field "inner" parse_exp_list o in
    Ok (DeclStmt children)
  | "LocationAliasStmt" ->
    let* s = with_field "source" parse_exp o in
    let* t = with_field "target" parse_exp o in
    let* o = with_field "offset" parse_exp o in
    Ok (LocationAliasStmt {source=s; target=t; offset=o})
  | "CompoundStmt" ->
    let* children : c_stmt list = with_field "inner" (fun (i:json) ->
      match i with
      | `Assoc _ -> let* o = parse_stmt i in Ok [o]
      | _ -> parse_stmt_list i
    ) o in
    Ok (CompoundStmt children)
  | "BreakStmt" ->
    Ok BreakStmt
  | "ForEachStmt" ->
    let* v = with_field "var" parse_variable o in
    let* r = with_field "range" parse_range o in
    let* b = with_field "body" parse_stmt o in
    Ok (ForEachStmt {var=v; range=r; body=b})
  | _ ->
    let* e = parse_exp j in
    Ok (CExp e)

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
      (fun idx k e -> Because ("error parsing kernel " ^ (string_of_int idx), k, e))

(* ------------------------------------------------------------------------ *)


let exp_to_s: c_exp -> string =
  let rec exp_to_s : c_exp -> string =
    function
    | FloatingLiteral f -> string_of_float f
    | IntegerLiteral i -> string_of_int i
    | ConditionalOperator c ->
      "(" ^ exp_to_s c.cond ^ ") ? (" ^
            exp_to_s c.then_expr ^ ") : (" ^
            exp_to_s c.else_expr ^ ")"
    | BinaryOperator b -> "(" ^ exp_to_s b.lhs ^ ") " ^ b.opcode ^ " (" ^ exp_to_s b.rhs ^ ")"
    | MemberExpr m -> "("^ exp_to_s m.base  ^ ")." ^ m.name
    | InitListExpr i -> exp_list_to_s i.args
    | ArraySubscriptExpr b -> exp_to_s b.lhs ^ "[" ^ exp_to_s b.rhs ^ "]"
    | CXXOperatorCallExpr c -> exp_to_s c.func ^ "(" ^ exp_list_to_s c.args  ^ ")"
    | CXXConstructExpr c -> "ctor"
    | CXXBoolLiteralExpr b -> if b then "true" else "false";
    | CallExpr c -> exp_to_s c.func ^ "(" ^ exp_list_to_s c.args  ^ ")"
    | VarDecl v ->
      var_name v.name ^ (
      match v.init with
      | Some e -> " = " ^ exp_to_s e
      | None -> ""
      )
    | UnresolvedLookupExpr v -> var_name v.name
    | NonTypeTemplateParmDecl v -> var_name v.name
    | CXXMethodDecl v -> var_name v.name
    | FunctionDecl v -> var_name v.name
    | ParmVarDecl v -> var_name v.name
    | UnaryOperator u -> u.opcode ^ exp_to_s u.child
    | Unknown s -> Yojson.Basic.pretty_to_string s
  and exp_list_to_s (l:c_exp list): string =
    List.map exp_to_s l |> Common.join ", "
  in exp_to_s

let range_to_s (r:c_range) : string =
  exp_to_s r.init ^ " .. " ^ exp_to_s r.upper_bound ^ "; " ^ r.opcode ^ exp_to_s r.step

let stmt_to_s: c_stmt -> PPrint.t list =
  let open PPrint in
  let rec stmt_to_s : c_stmt -> PPrint.t list =
    function
    | BreakStmt -> [Line "break;"]
    | SyncStmt -> [Line "sync;"]
    | AccessStmt _ -> [Line "access;"]
    | AssertStmt b -> [Line ("assert (" ^ (exp_to_s b) ^ ");")]
    | LocationAliasStmt l -> [Line (exp_to_s l.target ^ " = " ^ exp_to_s l.source ^ " + " ^ exp_to_s l.offset)]
    | ForEachStmt {var=v; range=r; body=b} ->
      [ Line ("foreach " ^ (var_name v) ^ " in " ^ range_to_s r ^ " {");
        Block (stmt_to_s b); Line "}"]
    | WhileStmt {cond=b; body=s} -> [
        Line ("while (" ^ exp_to_s b ^ ") {");
        Block (stmt_to_s s);
        Line "}"
      ]
    | IfStmt {cond=b; then_stmt=s1; else_stmt=s2} -> [
        Line ("if (" ^ exp_to_s b ^ ") {");
        Block (stmt_to_s s1);
        Line "} else {";
        Block (stmt_to_s s2);
        Line "}"
      ]
    | CompoundStmt l -> [Line "{"; Block (List.concat_map stmt_to_s l); Line "}"]
    | DeclStmt d -> [Line "decl {"; Block (List.map (fun e -> Line (exp_to_s e)) d); Line "}"]
    | CExp e -> [Line (exp_to_s e)]
  in
  stmt_to_s

let kernel_to_s (k:c_kernel) : PPrint.t list =
  let open PPrint in
  stmt_to_s k.code

let print_kernel (k: c_kernel) : unit =
  PPrint.print_doc (kernel_to_s k)

(* ------------------------------------------------------------------------ *)

let () =
  match Yojson.Basic.from_channel stdin |> parse_kernels with
  | Ok ks ->
    List.iteri (fun i k ->
      "Kernel: " ^ string_of_int i |> print_endline;
      print_kernel k
    ) ks
  | Error e -> Rjson.print_j_error e

