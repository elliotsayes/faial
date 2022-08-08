module StackTrace = Common.StackTrace

open Exp
open Serialize
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

type c_type = json
type c_var = {name: variable; ty: c_type}
type c_exp =
  | SizeOfExpr of c_type
  | CXXNewExpr of {arg: c_exp; ty: c_type}
  | CXXDeleteExpr of {arg: c_exp; ty: c_type}
  | RecoveryExpr of c_type
  | CharacterLiteral of int
  | ArraySubscriptExpr of c_array_subscript
  | BinaryOperator of c_binary
  | CallExpr of {func: c_exp; args: c_exp list; ty: c_type}
  | ConditionalOperator of {cond: c_exp; then_expr: c_exp; else_expr: c_exp; ty: c_type}
  | CXXConstructExpr of {args: c_exp list; ty: c_type}
  | CXXBoolLiteralExpr of bool
  | CXXMethodDecl of c_var
  | CXXOperatorCallExpr of {func: c_exp; args: c_exp list; ty: c_type}
  | FloatingLiteral of float
  | FunctionDecl of c_var
  | IntegerLiteral of int
  | NonTypeTemplateParmDecl of c_var
  | MemberExpr of {name: string; base: c_exp; ty: c_type}
  | ParmVarDecl of c_var
  | UnaryOperator of {opcode: string; child: c_exp; ty: c_type}
  | VarDecl of c_var
  | EnumConstantDecl of c_var
  | UnresolvedLookupExpr of {name: variable; tys: c_type list}
and c_binary = {opcode: string; lhs: c_exp; rhs: c_exp; ty: c_type}
and c_array_subscript = {lhs: c_exp; rhs: c_exp; ty: c_type}

module Init = struct
  type t =
    | InitListExpr of {ty: c_type; args: c_exp list}
    | IExp of c_exp
  let map_expr (f:c_exp -> c_exp) : t -> t =
    function
    | InitListExpr {ty=ty; args=l} ->
      InitListExpr {ty=ty; args=List.map f l}
    | IExp e -> IExp (f e)
end

type c_init = Init.t

let parse_type (j:Yojson.Basic.t) : Ctype.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* ty = with_field "qualType" cast_string o in
  Ok (Ctype.make ty)


let c_attr (k:string) : string =
  " __attribute__((" ^ k ^ "))"

let c_attr_shared = c_attr "shared"
let c_attr_global = c_attr "global"
let c_attr_device = c_attr "device"

module Decl = struct
  type t = {
    name: variable;
    ty: c_type;
    init: c_init option;
    attrs: string list
  }
  let name (x:t) : variable = x.name
  let ty (x:t) : c_type = x.ty
  let init (x:t) : c_init option = x.init
  let attrs (x:t) : string list = x.attrs  
  let is_shared (x:t) : bool =
    List.mem c_attr_shared x.attrs

  let map_expr (f: c_exp -> c_exp) (x:t) : t =
    { x with init=x.init |> Option.map (Init.map_expr f) }

  let is_array (x:t) : bool =
    match parse_type x.ty with
    | Ok ty -> Ctype.is_array ty
    | Error _ -> false    
end

type c_decl = Decl.t

type c_for_init =
  | ForDecl of c_decl list
  | ForExp of c_exp

type c_stmt =
  | BreakStmt
  | GotoStmt
  | ReturnStmt
  | ContinueStmt
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

module VisitExp = struct
  type 'a t =
    | SizeOf of c_type
    | CXXNew of {arg: 'a; ty: c_type}
    | CXXDelete of {arg: 'a; ty: c_type}
    | Recovery of c_type
    | CharacterLiteral of int
    | ArraySubscript of {lhs: 'a; rhs: 'a; ty: c_type}
    | BinaryOperator of {opcode: string; lhs: 'a; rhs: 'a; ty: c_type}
    | Call of {func: 'a; args: 'a list; ty: c_type}
    | ConditionalOperator of {cond: 'a; then_expr: 'a; else_expr: 'a; ty: c_type}
    | CXXConstruct of {args: 'a list; ty: c_type}
    | CXXBoolLiteral of bool
    | CXXMethodDecl of c_var
    | CXXOperatorCall of {func: 'a; args: 'a list; ty: c_type}
    | FloatingLiteral of float
    | FunctionDecl of c_var
    | IntegerLiteral of int
    | NonTypeTemplateParmDecl of c_var
    | Member of {name: string; base: 'a; ty: c_type}
    | ParmVarDecl of c_var
    | UnaryOperator of {opcode: string; child: 'a; ty: c_type}
    | VarDecl of c_var
    | EnumConstantDecl of c_var
    | UnresolvedLookup of {name: variable; tys: c_type list}

  let rec fold (f: 'a t -> 'a) : c_exp -> 'a =
    function
    | SizeOfExpr e -> f (SizeOf e)
    | CXXNewExpr e -> f (CXXNew {arg=fold f e.arg; ty=e.ty})
    | CXXDeleteExpr e -> f (CXXDelete {arg=fold f e.arg; ty=e.ty})
    | RecoveryExpr e -> f (Recovery e)
    | CharacterLiteral e -> f (CharacterLiteral e)
    | ArraySubscriptExpr e -> f (ArraySubscript {lhs=fold f e.lhs; rhs=fold f e.rhs; ty=e.ty})
    | BinaryOperator e -> f (BinaryOperator {
        opcode=e.opcode;
        lhs=fold f e.lhs;
        rhs=fold f e.rhs;
        ty=e.ty
      })
    | CallExpr e -> f (Call {func=fold f e.func; args=List.map (fold f) e.args; ty=e.ty})
    | ConditionalOperator e -> f (ConditionalOperator {
        cond=fold f e.cond;
        then_expr=fold f e.then_expr;
        else_expr=fold f e.else_expr;
        ty=e.ty
      })
    | CXXConstructExpr e -> f (CXXConstruct {args=List.map (fold f) e.args; ty=e.ty})
    | CXXBoolLiteralExpr e -> f (CXXBoolLiteral e)
    | CXXMethodDecl e -> f (CXXMethodDecl e)
    | CXXOperatorCallExpr e -> f (CXXOperatorCall {
        func=fold f e.func;
        args=List.map (fold f) e.args;
        ty=e.ty
      })
    | FloatingLiteral e -> f (FloatingLiteral e)
    | FunctionDecl e -> f (FunctionDecl e)
    | IntegerLiteral e -> f (IntegerLiteral e)
    | NonTypeTemplateParmDecl e -> f (NonTypeTemplateParmDecl e)
    | MemberExpr e -> f (Member {
        name=e.name;
        base=fold f e.base;
        ty=e.ty
      })
    | ParmVarDecl e -> f (ParmVarDecl e)
    | UnaryOperator e -> f (UnaryOperator {
        opcode=e.opcode;
        child=fold f e.child;
        ty=e.ty
      })
    | VarDecl e -> f (VarDecl e)
    | EnumConstantDecl e -> f (EnumConstantDecl e)
    | UnresolvedLookupExpr e ->
      f (UnresolvedLookup {name=e.name; tys=e.tys})

  let rec map (f: c_exp -> c_exp) (e: c_exp) : c_exp =
    let ret : c_exp -> c_exp = map f in 
    match e with
      | FloatingLiteral _
      | IntegerLiteral _
      | ParmVarDecl _
      | CharacterLiteral _
      | FunctionDecl _
      | NonTypeTemplateParmDecl _
      | CXXMethodDecl _ 
      | VarDecl _
      | EnumConstantDecl _
      | RecoveryExpr _
      | SizeOfExpr _
      | UnresolvedLookupExpr _
      | CXXBoolLiteralExpr _
        -> f e
      | CXXNewExpr {arg=a; ty=ty} ->
        f (CXXNewExpr {arg=ret a; ty=ty})
      | CXXDeleteExpr {arg=a; ty=ty} ->
        f (CXXDeleteExpr {arg=ret a; ty=ty})
      | ArraySubscriptExpr {lhs=e1; rhs=e2; ty=ty} ->
        f (ArraySubscriptExpr {lhs=ret e1; rhs=ret e2; ty=ty})
      | BinaryOperator {opcode=o; lhs=e1; rhs=e2; ty=ty} ->
        f (BinaryOperator {opcode=o; lhs=ret e1; rhs=ret e2; ty=ty})
      | CallExpr {func=e; args=l; ty=ty} ->
        f (CallExpr {func=f e; args=List.map ret l; ty=ty})
      | ConditionalOperator {cond=e1;then_expr=e2;else_expr=e3; ty=ty} ->
        f (ConditionalOperator {cond=f e1;then_expr=ret e2;else_expr=ret e3; ty=ty})
      | CXXConstructExpr  {args=l; ty=ty} ->
        f (CXXConstructExpr {args=List.map ret l; ty=ty})
      | CXXOperatorCallExpr {func=e; args=l; ty=ty} ->
        f (CXXOperatorCallExpr {func=ret e; args=List.map ret l; ty=ty})
      | MemberExpr {name=x; base=e; ty=ty} ->
        f (MemberExpr {name=x; base=ret e; ty=ty})
      | UnaryOperator {opcode=o; child=e; ty=ty} ->
        f (UnaryOperator {opcode=o; child=ret e; ty=ty})

end

module VisitStmt = struct
  type 'a t =
    | Break
    | Goto
    | Return
    | Continue
    | If of {cond: c_exp; then_stmt: 'a; else_stmt: 'a}
    | Compound of 'a list
    | Decl of c_decl list
    | While of {cond: c_exp; body: 'a}
    | For of {init: c_for_init option; cond: c_exp option; inc: c_exp option; body: 'a}
    | Do of {cond: c_exp; body: 'a}
    | Switch of {cond: c_exp; body: 'a}
    | Default of 'a
    | Case of {case: c_exp; body: 'a}
    | SExp of c_exp

  let rec fold (f: 'a t -> 'a) : c_stmt -> 'a =
    function
    | BreakStmt -> f Break
    | GotoStmt -> f Goto
    | ReturnStmt -> f Return
    | ContinueStmt -> f Continue
    | IfStmt c -> f (If {
        cond=c.cond;
        then_stmt=fold f c.then_stmt;
        else_stmt=fold f c.else_stmt;
      })
    | CompoundStmt l -> f (Compound (List.map (fold f) l))
    | DeclStmt l -> f (Decl l)
    | WhileStmt w -> f (While {
        cond=w.cond;
        body=fold f w.body;
      })
    | ForStmt s -> f (For {
        init=s.init;
        cond=s.cond;
        inc=s.inc;
        body=fold f s.body;
      })
    | DoStmt s -> f (Do {
        cond=s.cond;
        body=fold f s.body;
      })
    | SwitchStmt s -> f (Switch {
        cond=s.cond;
        body=fold f s.body;
      })
    | DefaultStmt s -> f (Default (fold f s))
    | CaseStmt s -> f (Case {case=s.case; body=fold f s.body})
    | SExp e -> f (SExp e)

  let to_expr_seq: c_stmt -> c_exp Seq.t =
    let init_to_expr : c_init -> c_exp Seq.t =
      function
      | InitListExpr l -> List.to_seq l.args
      | IExp e -> Seq.return e
    in
    fold (function
      | Break | Goto | Return | Continue -> Seq.empty
      | If {cond=c; then_stmt=s1; else_stmt=s2} ->
        Seq.return c
        |> Seq.append s1
        |> Seq.append s2
      | Compound l -> List.to_seq l |> Seq.concat
      | Decl d ->
        List.to_seq d
        |> Seq.concat_map (fun d ->
          let open Decl in
          Option.to_seq d.init
          |> Seq.concat_map init_to_expr 
        )
      | While {cond=c; body=b}
      | Do {cond=c; body=b}
      | Switch {cond=c; body=b}
      | Case {case=c; body=b}
        -> Seq.cons c b
      | For s ->
        Option.to_seq s.init
        |> Seq.concat_map (function
          | ForDecl l ->
            List.to_seq l
            |> Seq.concat_map (fun x -> Option.to_seq (Decl.init x) |> Seq.concat_map init_to_expr) 
          | ForExp e -> Seq.return e
        )
      | Default s -> s
      | SExp e -> Seq.return e
    )

end

type c_param = {name: variable; is_used: bool; is_shared: bool; ty: c_type}

type c_type_param =
  | PTemplateTypeParmDecl of variable
  | PNonTypeTemplateParmDecl of {name: variable; ty: c_type} 

module KernelAttr = struct
  type t =
    | Default
    | Auxiliary

  let to_string : t -> string =
    function
    | Default -> "__global__"
    | Auxiliary -> "__device__"

  let is_global : t -> bool =
    function
    | Default -> true
    | Auxiliary -> false

  let is_device : t -> bool =
    function
    | Default -> false
    | Auxiliary -> true

  let parse (x:string) : t option =
    if x = c_attr_global then Some Default
    else if x = c_attr_device then Some Auxiliary
    else None

  let can_parse (x:string) : bool =
    parse x |> Option.is_some

end

type c_kernel = {
  name: string;
  code: c_stmt;
  type_params: c_type_param list;
  params: c_param list;
  attribute: KernelAttr.t;
}

type c_def =
  | Kernel of c_kernel
  | Declaration of c_decl

type c_program = c_def list

(* ------------------------------------------------------------------- *)

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

let rec exp_type (e:c_exp) : c_type =
  match e with
  | SizeOfExpr _ -> Ctype.j_int_type
  | CXXNewExpr c -> c.ty
  | CXXDeleteExpr c -> c.ty
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
  | RecoveryExpr ty -> ty

let exp_name =
  function
  | SizeOfExpr _ -> "SizeOfExpr"
  | CXXNewExpr _ -> "CXXNewExpr"
  | CXXDeleteExpr _ -> "CXXNewExpr"
  | RecoveryExpr _ -> "RecoveryExpr"
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
  let e = with_field "end" parse_position o |> unwrap_or s in
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
    let* l = parse_location range in
    Ok (LocVariable (l, name))
  | None -> Ok (Variable name)
  

let compound (ty:c_type) (lhs:c_exp) (opcode:string) (rhs:c_exp) : c_exp =
  BinaryOperator {
    ty=ty;
    opcode="=";
    lhs=lhs;
    rhs=BinaryOperator {
      ty=ty;
      opcode=opcode;
      lhs=lhs;
      rhs=rhs
    }
  }

let is_invalid (o: j_object) : bool =
  let open Rjson in
  with_opt_field "isInvalid" cast_bool o
  |> unwrap_or None
  |> Ojson.unwrap_or false

let rec parse_exp (j:json) : c_exp j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | _ when is_invalid o ->
    (* Unknown value *)
    let* ty = get_field "type" o in
    Ok (RecoveryExpr ty)

  | "CXXDefaultArgExpr"
  | "ImplicitValueInitExpr"
  | "CXXNullPtrLiteralExpr"
  | "StringLiteral"
  | "DependentScopeDeclRefExpr"
  | "RecoveryExpr" ->
    (* Unknown value *)
    let* ty = get_field "type" o in
    Ok (RecoveryExpr ty)

  | "CharacterLiteral" ->
    let* i = with_field "value" cast_int o in
    Ok (CharacterLiteral i)

  | "CXXConstCastExpr"
  | "CXXReinterpretCastExpr"
  | "PackExpansionExpr"
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
    with_field "referencedDecl" (fun new_j ->
      (* Propagate provenance *)
      let new_j = match new_j, List.assoc_opt "range" o with
      | `Assoc new_o, Some range ->
        `Assoc (("range", range)::new_o)
      | _, _ -> new_j
      in 
      parse_exp new_j
    ) o

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

  | "UnaryExprOrTypeTraitExpr" ->
    let* ty = get_field "type" o in
    Ok (SizeOfExpr ty)

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

  | "CXXNewExpr" ->
    let* arg = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    Ok (CXXNewExpr {arg=arg; ty=ty})

  | "CXXDeleteExpr" ->
    let* arg = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    Ok (CXXDeleteExpr {arg=arg; ty=ty})

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
      | Some (opcode, "") -> Ok (compound ty lhs opcode rhs)
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

  | "CXXMemberCallExpr"
  | "CXXOperatorCallExpr" ->
    let* (func, args) = with_field "inner" (fun j ->
      let* h, t = cast_cons j in
      let* func = wrap parse_exp (fun _ -> "func", j) h in
      let* args = wrap (map parse_exp) (fun _ -> "args", j) t in
      Ok (func, args)
    ) o in
    let* ty = get_field "type" o in
    Ok (
      match func, args with
      | CXXMethodDecl {name=n}, [lhs; rhs] when var_name n = "operator=" ->
        BinaryOperator {lhs=lhs; opcode="="; rhs=rhs; ty=exp_type lhs}
      | (UnresolvedLookupExpr {name=n}, [lhs; rhs])
      | (FunctionDecl {name=n}, [lhs; rhs]) ->
        (match var_name n with
          | "operator-=" -> compound ty lhs "-" rhs  
          | "operator+=" -> compound ty lhs "+" rhs  
          | "operator*=" -> compound ty lhs "*" rhs
          | "operator/=" -> compound ty lhs "/" rhs
          | "operator%=" -> compound ty lhs "%" rhs
          | "operator^=" -> compound ty lhs "^" rhs
          | "operator&=" -> compound ty lhs "&" rhs
          | "operator|=" -> compound ty lhs "|" rhs
          | "operator<<=" -> compound ty lhs "<<" rhs
          | "operator>>=" -> compound ty lhs ">>" rhs
          | _ -> CXXOperatorCallExpr {func=func; args=args; ty=ty}
        )
      | _ -> CXXOperatorCallExpr {func=func; args=args; ty=ty}
    )

  | "CallExpr" ->
    let* (func, args) = with_field "inner" (fun j ->
      let* h, t = cast_cons j in
      let* func = wrap parse_exp (fun _ -> "func", j) h in
      let* args = wrap (map parse_exp) (fun _ -> "args", j) t in
      Ok (func, args)
    ) o in
    let* ty = get_field "type" o in
    Ok (CallExpr {func=func; args=args; ty=ty})

  | "CXXBindTemporaryExpr"
  | "CXXFunctionalCastExpr"
  | "MaterializeTemporaryExpr" ->
    let* body = with_field "inner" (cast_list_1 parse_exp) o in
    Ok body

  | "CXXTemporaryObjectExpr"
  | "InitListExpr"
  | "CXXUnresolvedConstructExpr"
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
  | "ParenListExpr"
  | "InitListExpr" ->
    let* ty = get_field "type" o in
    let* args = with_field_or "inner" (cast_map parse_exp) [] o in
    let open Init in
    Ok (InitListExpr {ty=ty; args=args})

  | _ ->
    let* e = parse_exp j in
    let open Init in
    Ok (IExp e)

let parse_attr (j:Yojson.Basic.t) : string j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  with_field "value" cast_string o

let is_valid_j : json -> bool =
  function
  | `Assoc o ->
    (match Rjson.get_kind o with
      | Error _ | Ok "FullComment" -> false
      | Ok _ -> true)
  | _ -> false

let parse_decl (j:json) : c_decl option j_result =
  let open Rjson in
  let* o = cast_object j in
  if is_invalid o then Ok None
  else (
    let* v = parse_variable j in
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
          | "CUDASharedAttr" -> true
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
    Ok (Some {Decl.name=v; Decl.ty=ty; Decl.init=init; Decl.attrs=attrs})
  )

let parse_for_init (j:json) : c_for_init j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "DeclStmt" ->
    let* ds = with_field "inner" (cast_map parse_decl) o in
    Ok (ForDecl (Common.flatten_opt ds))
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
    let has_typedecl : bool =
      let has_typedecl : bool j_result =
        let* children = get_field "inner" o in
        let* l = cast_list children in
        let* o = get_index 0 l >>= cast_object in
        let* k = get_kind o in
        Ok (k = "TypedefDecl" || k = "EnumDecl" || k = "TypeAliasDecl")
      in
      Rjson.unwrap_or false has_typedecl
    in
    if has_typedecl then
      Ok (CompoundStmt [])
    else
      let* children = with_field "inner" (cast_map parse_decl) o in
      Ok (DeclStmt (children |> Common.flatten_opt))
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
  | Some "ContinueStmt" ->
    Ok ContinueStmt
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
  | Some "FullComment"
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
  let* o = cast_object j in
  let* v = parse_variable j in
  let* ty = get_field "type" o in
  let* is_refed = with_field_or "isReferenced" cast_bool false o in
  let* is_used =  with_field_or "isUsed" cast_bool false o in
  let* is_shared = with_field_or "shared" cast_bool false o in
  Ok {name=v; is_used=(is_refed || is_used); is_shared=is_shared; ty=ty}

let j_filter_kind (f:string -> bool) (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let res =
    let* o = cast_object j in
    let* k = get_kind o in
    Ok (f k)
  in
  res |> unwrap_or false

let wrap_error (msg:string) (j:Yojson.Basic.t): 'a j_result -> 'a j_result =
    function
    | Ok e -> Ok e
    | Error e -> Rjson.because msg j e


let parse_kernel (type_params:c_type_param list) (j:Yojson.Basic.t) : c_kernel j_result =
  let open Rjson in
  (
    let* o = cast_object j in
    let* inner = with_field "inner" cast_list o in
    let attrs, inner =
      inner
      |> List.partition
        (j_filter_kind (String.ends_with ~suffix:"Attr"))
    in
    let ps, body =
      inner
      |> List.partition
        (j_filter_kind (fun k -> k = "ParmVarDecl" || k = "TemplateArgument"))
    in
    let* attrs = map parse_attr attrs in
    (* we can safely convert the option with Option.get because parse_kernel
       is only invoked when we are able to parse *)
    let m: KernelAttr.t = List.find_map KernelAttr.parse attrs |> Option.get in
    let* body: c_stmt list = parse_stmt_list (Yojson.Basic.(`List body)) in
    let body = match body with
      | [s] -> s
      | _ -> CompoundStmt body
    in
    let* name: string = with_field "name" cast_string o in
    (* Parameters may be faulty, recover: *)
    let ps = List.map parse_param ps |> List.concat_map Result.to_list in
    Ok {
      name = name;
      code = body;
      params = ps;
      type_params = type_params;
      attribute = m;
    }
  ) |> wrap_error "Kernel" j

(* Function that checks if a variable is of type array and is being used *)
let has_array_type (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let is_array =
    let* o = cast_object j in
    let is_used = match List.assoc_opt "isUsed" o with
      | Some (`Bool true) -> true
      | _ -> false
    in
    let* k = get_kind o in
    let* ty = get_field "type" o in
    let* ty = parse_type ty in
    Ok (is_used && Ctype.is_array ty)
  in
  is_array |> Rjson.unwrap_or false


let is_kernel (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let is_kernel =
    let* o = cast_object j in
    let* k = get_kind o in
    if k = "FunctionDecl" then (
      let* name: string = with_field "name" cast_string o in
      let* inner = with_field "inner" cast_list o in
      let attrs, inner =
        inner
        |> List.partition (j_filter_kind (String.ends_with ~suffix:"Attr"))
      in
      (* Try to parse attrs *)
      let attrs = attrs
        |> Common.map_opt (fun j ->
          parse_attr j >>= (fun a -> Ok (Some a))
          |> unwrap_or None
        )
      in
      let params, inner =
        inner
        |> List.partition (j_filter_kind (fun k -> k = "ParmVarDecl"))
      in
      Ok (match List.find_map KernelAttr.parse attrs with
      | Some KernelAttr.Default -> true
      | None -> false
      | Some KernelAttr.Auxiliary ->
        (* We only care about __device__ functions that manipulate arrays *)
        List.exists has_array_type params
      )
    ) else Ok false
  in
  is_kernel |> unwrap_or false

let parse_type_param (j:Yojson.Basic.t) : c_type_param option j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  match k with
  | "TemplateTypeParmDecl" ->
    let* name = parse_variable j in
    Ok (Some (PTemplateTypeParmDecl name))
  | "NonTypeTemplateParmDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Some (PNonTypeTemplateParmDecl {name=name; ty=ty}))
  | _ -> Ok None


let rec parse_def (j:Yojson.Basic.t) : c_def list j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  let parse_k (type_params:c_type_param list) (j:Yojson.Basic.t) : c_def list j_result =
    if is_kernel j then
      let* k = parse_kernel type_params j in
      Ok [Kernel k]
    else Ok []
  in
  match k with
  | "FunctionTemplateDecl" ->
    (* Given a list of inners, we parse from left-to-right the
       template parameters first.
       If we cannot find parse a template parameter, then
       we try to parse a function declaration. In some cases we
       might even have some more parameters after the function
       declaration, but those are discarded, as I did not understand
       what they are for. *)
    let rec handle (type_params:c_type_param list): Yojson.Basic.t list -> c_def list j_result =
      function
      | [] -> root_cause "Error parsing FunctionTemplateDecl: no FunctionDecl found" j
      | j :: l ->
        let* p = parse_type_param j in
        (match p with
        | Some p -> handle (p::type_params) l
        | None -> parse_k type_params j)
    in
    let* inner = with_field "inner" cast_list o in
    handle [] inner
  | "FunctionDecl" -> parse_k [] j
  | "VarDecl" ->
    (match parse_decl j with
    | Ok (Some d) ->
      Ok ([Declaration d])
    | _ -> Ok [])
  | "LinkageSpecDecl"
  | "NamespaceDecl" ->
    let* defs = with_field_or "inner" (cast_map parse_def) [] o in
    Ok (List.concat defs)
  | _ ->
    Ok []


(* ------------------------------------------------- *)

let rewrite_shared_arrays: c_program -> c_program =
  (* Rewrites expressions: when it finds a variable that has been defined as
     a shared variable, we replace that by an array subscript:
     x becomes x[0] *)
  let rw_exp (vars:VarSet.t) (e:c_exp) : c_exp =
    if VarSet.is_empty vars then e else
    e |> VisitExp.map (fun e ->
      match e with
      | VarDecl x ->
        if VarSet.mem x.name vars
        then ArraySubscriptExpr {lhs=VarDecl x; rhs=IntegerLiteral 0; ty=x.ty}
        else e
      | _ -> e)
  in
  (* When rewriting a variable declaration, we must return as the side-effect
     the shadowing of the available variables when it makes sense *) 
  let rw_decl (vars:VarSet.t) (d:Decl.t) : VarSet.t * Decl.t =
    let vars =
      if Decl.is_shared d && not (Decl.is_array d) then
        VarSet.add d.name vars
      else
        VarSet.remove d.name vars
    in
    (vars, Decl.map_expr (rw_exp vars) d)
  in
  (* This is just a monadic map *)
  let rec rw_list: 'a. (VarSet.t -> 'a -> VarSet.t * 'a) -> VarSet.t -> 'a list -> VarSet.t * 'a list =
    fun f vars l ->
        match l with
        | [] -> (vars, [])
        | x :: l ->
          let (vars, x) = f vars x in
          let (vars, l) = rw_list f vars l in
          (vars, x::l)
  in
  (* We now rewrite statements *)
  let rw_stmt (vars:VarSet.t) (s:c_stmt) : c_stmt =
    let rec rw_s (vars:VarSet.t) (s: c_stmt) : VarSet.t * c_stmt =
      let ret (s: c_stmt) : c_stmt = rw_s vars s |> snd in
      let rw_e: c_exp -> c_exp = rw_exp vars in
      match s with
      | BreakStmt
      | GotoStmt
      | ReturnStmt
      | ContinueStmt
        -> (vars, s)
      | IfStmt {cond=c; then_stmt=s1; else_stmt=s2} ->
        (vars, IfStmt {cond=rw_e c; then_stmt=ret s1; else_stmt=ret s2})
      | CompoundStmt l ->
        (* This is one of the interesting cases, since we
           propagate scoping through the monadic map (rw_list). *)
        (vars, CompoundStmt (rw_list rw_s vars l |> snd))
      | DeclStmt l ->
        (* Variable declaration introduces a scope *)
        let (vars, l) = rw_list rw_decl vars l in
        (vars, DeclStmt l)
      | WhileStmt {cond=e; body=s} ->
        (vars, WhileStmt {cond=rw_e e; body=ret s})
      | ForStmt {init=e1; cond=e2; inc=e3; body=s} ->
        (* The init may create a scope *)
        let (vars_body, e1) = match e1 with
        | None -> (vars, None)
        | Some (ForDecl l) ->
          let (vars, l) = rw_list rw_decl vars l in
          (vars, Some (ForDecl l))
        | Some (ForExp e) -> (vars, Some (ForExp (rw_e e)))
        in
        let e2 = Option.map (rw_exp vars_body) e2 in
        let e3 = Option.map (rw_exp vars_body) e3 in
        (vars, ForStmt {init=e1; cond=e2; inc=e3; body=rw_s vars_body s |> snd})
      | DoStmt {cond=e; body=s} ->
        (vars, DoStmt {cond=rw_e e; body=ret s})
      | SwitchStmt {cond=e; body=s} ->
        (vars, SwitchStmt {cond=rw_e e; body=ret s})
      | DefaultStmt s ->
        (vars, DefaultStmt (ret s))
      | CaseStmt  {case=e; body=s} ->
        (vars, CaseStmt {case=rw_e e; body=ret s})
      | SExp e ->
        (vars, SExp (rw_e e))
    in
    rw_s vars s |> snd
  in
  let rec rw_p (vars:VarSet.t): c_program -> c_program =
    function
    | Declaration d :: p ->
      let vars = if Decl.is_shared d && not (Decl.is_array d)
        then VarSet.add d.name vars
        else vars
      in
      Declaration d :: rw_p vars p
    | Kernel k :: p -> Kernel { k with code = rw_stmt vars k.code } :: rw_p vars p
    | [] -> []
  in
  rw_p VarSet.empty

let parse_program ?(rewrite_shared_variables=true) (j:Yojson.Basic.t) : c_program j_result =
  let open Rjson in
  let* o = cast_object j in
  let* inner = with_field "inner" (cast_map parse_def) o in
  let p = List.concat inner in
  Ok (if rewrite_shared_variables then rewrite_shared_arrays p else p)

let type_to_str (j:Yojson.Basic.t) : string =
  match parse_type j with
  | Ok ty -> Ctype.to_string ty
  | Error _ -> "?"

(* ------------------------------------------------------------------------ *)

let list_to_s (f:'a -> string) (l:'a list) : string =
  List.map f l |> Common.join ", "

let exp_to_s ?(modifier:bool=false) ?(provenance:bool=false) ?(types:bool=false) : c_exp -> string =
  let attr (s:string) : string =
    if modifier
    then "@" ^ s ^ " "
    else ""
  in
  let opcode (o:string) (j:Yojson.Basic.t) : string =
    if types
    then "(" ^ o ^ "." ^ type_to_str j ^ ")"
    else o
  in
  let var_name: variable -> string =
    if provenance
    then var_repr
    else var_name
  in
  let rec exp_to_s: c_exp -> string =
    let par (e: c_exp) : string =
      match e with
      | BinaryOperator _
      | ConditionalOperator _
        -> "(" ^ exp_to_s e ^ ")"
      | UnaryOperator _
      | CXXNewExpr _
      | CXXDeleteExpr _
      | FunctionDecl _
      | ParmVarDecl _
      | EnumConstantDecl _
      | NonTypeTemplateParmDecl _
      | UnresolvedLookupExpr _
      | VarDecl _
      | CallExpr _
      | CXXMethodDecl _
      | CXXOperatorCallExpr _
      | CXXConstructExpr _
      | CXXBoolLiteralExpr _
      | ArraySubscriptExpr _
      | MemberExpr _
      | IntegerLiteral _
      | CharacterLiteral _
      | RecoveryExpr _
      | FloatingLiteral _
      | SizeOfExpr _
        ->  exp_to_s e
    in
    function
    | SizeOfExpr ty -> "sizeof(" ^ type_to_str ty ^ ")"
    | CXXNewExpr c -> "new " ^ type_to_str c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
    | CXXDeleteExpr c -> "del " ^ par c.arg
    | RecoveryExpr _ -> "?"
    | FloatingLiteral f -> string_of_float f
    | CharacterLiteral i
    | IntegerLiteral i -> string_of_int i
    | ConditionalOperator c ->
      par c.cond ^ " ? " ^ par c.then_expr ^ " : " ^ par c.else_expr
    | BinaryOperator b ->
      par b.lhs ^ " " ^ opcode b.opcode b.ty ^ " " ^ par b.rhs
    | MemberExpr m -> par m.base  ^ "." ^ m.name
    | ArraySubscriptExpr b -> par b.lhs ^ "[" ^ exp_to_s b.rhs ^ "]"
    | CXXBoolLiteralExpr b -> if b then "true" else "false";
    | CXXConstructExpr c -> attr "ctor" ^ type_to_str c.ty ^ "(" ^ list_to_s exp_to_s c.args ^ ")" 
    | CXXOperatorCallExpr c -> exp_to_s c.func ^ "[" ^ list_to_s exp_to_s c.args  ^ "]"
    | CXXMethodDecl v -> attr "meth" ^ var_name v.name
    | CallExpr c -> par c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
    | VarDecl v -> var_name v.name
    | UnresolvedLookupExpr v -> attr "unresolv" ^ var_name v.name
    | NonTypeTemplateParmDecl v -> attr "tpl" ^ var_name v.name
    | FunctionDecl v -> attr "func" ^ var_name v.name
    | ParmVarDecl v -> attr "parm" ^ var_name v.name
    | EnumConstantDecl v -> attr "enum" ^ var_name v.name
    | UnaryOperator u -> u.opcode ^ par u.child
  in
  exp_to_s

let init_to_s : c_init -> string =
  function
  | InitListExpr i -> list_to_s exp_to_s i.args
  | IExp i -> exp_to_s i

let decl_to_s (d: c_decl): string =
  let i = match d.init with
    | Some e -> " = " ^ init_to_s e
    | None -> ""
  in
  let attr = if d.attrs = [] then "" else
    let attrs = Common.join " " d.attrs |> String.trim in
    attrs ^ " "
  in
  attr ^ type_to_str d.ty ^ " " ^ var_name d.name ^ i


let for_init_to_s (f:c_for_init) : string =
  match f with
  | ForDecl d -> list_to_s decl_to_s d
  | ForExp e -> exp_to_s e

let opt_for_init_to_s (o:c_for_init option) : string =
  match o with
  | Some o -> for_init_to_s o
  | None -> ""

let stmt_to_s ?(modifier:bool=false) ?(provenance:bool=false) : c_stmt -> PPrint.t list =
  let exp_to_s : c_exp -> string = exp_to_s ~modifier ~provenance in
  let opt_exp_to_s: c_exp option -> string =
    function
    | Some c -> exp_to_s c
    | None -> ""
  in
  let open PPrint in
  let rec stmt_to_s : c_stmt -> PPrint.t list =
    let ret l : PPrint.t list =
      match l with
      | [] -> [Line ";"]
      | [Line "{"; Block l; Line "}"]
      | l -> [Line "{"; Block l; Line "}"]
    in 
    let block (s:c_stmt) : PPrint.t list = ret (stmt_to_s s) in 
    function
    | ReturnStmt -> [Line "return"]
    | GotoStmt -> [Line "goto"]
    | BreakStmt -> [Line "break"]
    | ContinueStmt -> [Line "continue"]
    | ForStmt f -> [
        Line ("for (" ^ opt_for_init_to_s f.init ^ "; " ^ opt_exp_to_s f.cond ^ "; " ^ opt_exp_to_s f.inc ^ ")");
      ]
      @ block (f.body)
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
    | IfStmt {cond=b; then_stmt=s1; else_stmt=s2} ->
      let s1 = stmt_to_s s1 in
      let s2 = stmt_to_s s2 in
      if s1 = [] && s2 = [] then []
      else
        [Line ("if (" ^ exp_to_s b ^ ")")] @
        ret s1 @
        (if s2 = [] then [] else [ Line "else"; ] @ ret s2)
    | CompoundStmt [] -> []
    | CompoundStmt l ->
      let l = List.concat_map stmt_to_s l in
      if l = [] then [] else ret l
    | DeclStmt [] -> []
    | DeclStmt [d] -> [Line ("decl " ^ decl_to_s d)]
    | DeclStmt d -> [Line "decl {"; Block (List.map (fun e -> Line (decl_to_s e)) d); Line "}"]
    | SExp e -> [Line (exp_to_s e)]
  in
  stmt_to_s

let param_to_s (p:c_param) : string =
  let used = if p.is_used then "" else " unsed" in
  let shared = if p.is_shared then "shared " else "" in
  used ^ shared ^ var_name p.name

let type_param_to_s (p:c_type_param) : string =
  let name = match p with
  | PTemplateTypeParmDecl x -> x
  | PNonTypeTemplateParmDecl x -> x.name
  in
  var_name name

let kernel_to_s ?(modifier:bool=false) ?(provenance:bool=false) (k:c_kernel) : PPrint.t list =
  let tps = if k.type_params <> [] then "[" ^
      list_to_s type_param_to_s k.type_params ^
    "]" else ""
  in
  let open PPrint in
  [
    Line (KernelAttr.to_string k.attribute ^ " " ^ k.name ^
      " " ^ tps ^ "(" ^ list_to_s param_to_s k.params ^ ")");
  ]
  @
  stmt_to_s ~modifier ~provenance k.code

let def_to_s ?(modifier:bool=false) ?(provenance:bool=false) (d:c_def) : PPrint.t list =
  let open PPrint in
  match d with
  | Declaration d -> [Line (decl_to_s d ^ ";")]
  | Kernel k -> kernel_to_s ~modifier ~provenance k

let program_to_s ?(modifier:bool=false) ?(provenance:bool=false) (p:c_program) : PPrint.t list =
  List.concat_map (fun k -> def_to_s ~modifier ~provenance k @ [Line ""]) p

let print_program ?(modifier:bool=false) ?(provenance:bool=false) (p:c_program) : unit =
  PPrint.print_doc (program_to_s ~modifier ~provenance p)

