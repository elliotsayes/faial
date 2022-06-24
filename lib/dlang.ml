module StackTrace = Common.StackTrace

open Exp
open Serialize
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

type d_type = json
type d_var = Cast.c_var

type d_exp =
  | CharacterLiteral of int
  | BinaryOperator of d_binary
  | CallExpr of {func: d_exp; args: d_exp list; ty: d_type}
  | ConditionalOperator of {cond: d_exp; then_expr: d_exp; else_expr: d_exp; ty: d_type}
  | CXXConstructExpr of {args: d_exp list; ty: d_type}
  | CXXBoolLiteralExpr of bool
  | CXXMethodDecl of d_var
  | CXXOperatorCallExpr of {func: d_exp; args: d_exp list; ty: d_type}
  | FloatingLiteral of float
  | FunctionDecl of d_var
  | IntegerLiteral of int
  | NonTypeTemplateParmDecl of d_var
  | MemberExpr of {name: string; base: d_exp; ty: d_type}
  | ParmVarDecl of d_var
  | UnaryOperator of { opcode: string; child: d_exp; ty: d_type}
  | VarDecl of d_var
  | EnumConstantDecl of d_var
  | UnresolvedLookupExpr of {name: variable; tys: d_type list}
and d_binary = {opcode: string; lhs: d_exp; rhs: d_exp; ty: d_type}

type d_init =
  | CXXConstructExpr of {constructor: d_type; ty: d_type}
  | InitListExpr of {ty: d_type; args: d_exp list}
  | IExp of d_exp

type d_decl = {
  name: variable;
  ty: d_type;
  init: d_init option;
  attrs: string list
}

type d_for_init =
  | ForDecl of d_decl list
  | ForExp of d_exp

type d_subscript = {name: variable; index: d_exp list; ty: d_type}
type d_write = {target: d_subscript; source: d_exp}
type d_read = {target: variable; source: d_subscript}

type d_stmt =
  | WriteAccessStmt of d_write
  | ReadAccessStmt of d_read
  | BreakStmt
  | GotoStmt
  | ReturnStmt
  | IfStmt of {cond: d_exp; then_stmt: d_stmt; else_stmt: d_stmt}
  | CompoundStmt of d_stmt list
  | DeclStmt of d_decl list
  | WhileStmt of {cond: d_exp; body: d_stmt}
  | ForStmt of d_for
  | DoStmt of {cond: d_exp; body: d_stmt}
  | SwitchStmt of {cond: d_exp; body: d_stmt}
  | DefaultStmt of d_stmt
  | CaseStmt of {case: d_exp; body: d_stmt}
  | SExp of d_exp
and d_for = {init: d_for_init option; cond: d_exp option; inc: d_exp option; body: d_stmt}


type d_kernel = {
  name: string;
  code: d_stmt;
  params: Cast.c_param list;
}

type d_def =
  | Kernel of d_kernel
  | Declaration of d_var

type d_program = d_def list


(* ------------------------------------- *)
let rec d_stmt_rec (f: d_stmt -> unit) (stmt: d_stmt): unit = let loop = d_stmt_rec f in
 f stmt ; match stmt with
    | IfStmt {cond: d_exp; then_stmt: d_stmt; else_stmt: d_stmt} -> List.iter loop [then_stmt; else_stmt]
    | WhileStmt {cond: d_exp; body: d_stmt} -> loop body
    | DoStmt {cond: d_exp; body: d_stmt} -> loop body
    | SwitchStmt {cond: d_exp; body: d_stmt} -> loop body
    | CaseStmt {case: d_exp; body: d_stmt} -> loop body
    | ForStmt {init: d_for_init option; cond: d_exp option; inc: d_exp option; body: d_stmt} -> loop body
    | CompoundStmt stmts -> List.iter loop stmts
    | _ -> ()

let rec d_exp_rec (f: d_exp -> unit) (expr: d_exp): unit = let loop = d_exp_rec f in
 f expr ; match expr with
  | BinaryOperator {opcode: string; lhs: d_exp; rhs: d_exp; ty: d_type} -> List.iter loop [lhs; rhs]
  | CallExpr {func: d_exp; args: d_exp list; ty: d_type} -> List.iter loop (List.append [func] args)
  | ConditionalOperator {cond: d_exp; then_expr: d_exp; else_expr: d_exp; ty: d_type} ->
      List.iter loop [cond; then_expr; else_expr]
  | CXXConstructExpr {args: d_exp list; ty: d_type} -> List.iter loop args
  | CXXOperatorCallExpr {func: d_exp; args: d_exp list; ty: d_type} ->
      List.iter loop (List.append [func] args)
  | MemberExpr {name: string; base: d_exp; ty: d_type} -> loop base
  | UnaryOperator { opcode: string; child: d_exp; ty: d_type} -> loop child
  | _ -> ()

let for_dexp_in_dstmt (f: d_exp -> unit) (stmt: d_stmt) = let traversal = d_exp_rec f in
  d_stmt_rec (fun stmt' ->
    match stmt' with
      | WriteAccessStmt writeStmt -> traversal writeStmt.source
      | ReadAccessStmt readStmt -> List.iter traversal readStmt.source.index
      | IfStmt ifStmt -> traversal ifStmt.cond
      | DeclStmt decls -> List.iter (fun (decl: d_decl) -> decl.init |> Option.iter (fun decl -> match decl with
          | InitListExpr initListExpr -> List.iter traversal initListExpr.args
          | IExp d_exp' -> traversal d_exp'
          | _ -> ())
        ) decls
      | WhileStmt whileStmt -> traversal whileStmt.cond
      | ForStmt d_for' -> Option.iter traversal d_for'.cond
      | DoStmt doStmt -> traversal doStmt.cond
      | SwitchStmt switchStmt -> traversal switchStmt.cond
      | CaseStmt caseStmt -> traversal caseStmt.case
      | SExp d_exp' -> traversal d_exp'
      | _ -> ()
  ) stmt


let exp_name = function
| CharacterLiteral _ -> "CharacterLiteral"
| BinaryOperator _ -> "BinaryOperator"
| CallExpr _ -> "CallExpr"
| ConditionalOperator _ -> "ConditionalOperator"
| CXXConstructExpr _ -> "CXXConstructExpr"
| CXXBoolLiteralExpr _ -> "CXXBoolLiteralExpr"
| CXXMethodDecl _ -> "CXXMethodDecl"
| CXXOperatorCallExpr _ -> "CXXOperatorCallExpr"
| FloatingLiteral _ -> "FloatingLiteral"
| FunctionDecl _ -> "FunctionDecl"
| IntegerLiteral _ -> "IntegerLiteral"
| NonTypeTemplateParmDecl _ -> "NonTypeTemplateParmDecl"
| MemberExpr _ -> "MemberExpr"
| ParmVarDecl _ -> "ParmVarDecl"
| EnumConstantDecl _ -> "EnumConstantDecl"
| UnaryOperator _ -> "UnaryOperator"
| VarDecl _ -> "VarDecl"
| UnresolvedLookupExpr _ -> "UnresolvedLookupExpr"

let get_variable : d_exp -> variable option = function
| CXXMethodDecl {name=n}
| FunctionDecl {name=n}
| NonTypeTemplateParmDecl {name=n}
| ParmVarDecl {name=n}
| VarDecl {name=n}
| UnresolvedLookupExpr {name=n} -> Some n
| _ -> None


let init_to_exp (i:d_init) : d_exp list =
  match i with
  | CXXConstructExpr _ -> []
  | InitListExpr i -> i.args
  | IExp e -> [e]

let decl_to_exp (d:d_decl) : d_exp list =
  match d.init with
  | Some i -> init_to_exp i
  | None -> []

let for_init_to_exp (f:d_for_init) : d_exp list =
  match f with
  | ForDecl l -> List.fold_left
    (fun l d -> Common.append_rev (decl_to_exp d) l)
    []
    l
  | ForExp e -> [e]

let for_to_exp (f:d_for) : d_exp list =
  let l1 = f.init |> Option.map for_init_to_exp |> Ojson.unwrap_or [] in
  let l2 = f.cond |> Option.map (fun x -> [x]) |> Ojson.unwrap_or [] in
  let l3 = f.inc |> Option.map (fun x -> [x]) |> Ojson.unwrap_or [] in
  l1
  |> Common.append_rev l2
  |> Common.append_rev l3

let for_loop_vars (f:d_for) : variable list =
  let rec exp_var (e:d_exp) : variable list =
    match e with
    | BinaryOperator {lhs=l; opcode=","; rhs=r} ->
      exp_var l |> Common.append_rev (exp_var r)
    | BinaryOperator {lhs=l; opcode="="; rhs=r} ->
      (match get_variable l with
      | Some x -> [x]
      | None -> [])
    | _ -> []
  in
  match f.init with
  | Some (ForDecl l) -> List.map (fun (d:d_decl) -> d.name) l
  | Some (ForExp e) -> exp_var e
  | None -> []

(* ------------------------------------- *)


let rec exp_type (e:d_exp) : d_type =
  match e with
  | CharacterLiteral _ -> Ctype.j_char_type
  | BinaryOperator a -> a.ty
  | ConditionalOperator c -> exp_type c.then_expr
  | CXXBoolLiteralExpr _ -> Ctype.j_bool_type
  | CXXMethodDecl a -> a.ty
  | CXXConstructExpr c -> c.ty
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
(* ------------------------------------------------------------------------ *)

type ('s, 'a) state = 's -> 's * 'a

let state_pure (x:'a) : ('s, 'a) state = fun (st:'s) -> (st, x)

let state_bind (f:'a -> ('s, 'b) state) (eff1:('s, 'a) state) : ('s, 'b) state =
    fun (st1:'s) ->
      let (st2, x : 's * 'a) = eff1 st1 in
      f x st2


let state_map (f:'a -> ('s, 'b) state) (l:'a list) : ('s, 'b list) state =
  let rec handle_list (l:'a list) : ('s, 'b list) state =
    match l with
    | [] -> state_pure []
    | x::l ->
      fun st ->
      let (st, x) = f x st in
      let (st, l) = handle_list l st in
      state_pure (x::l) st
  in
  handle_list l

(* Monadic let *)
let (let*) = state_bind
(* Monadic pipe *)
let (>>=) = state_bind


module AccessState = struct
  type t = d_stmt list

  let counter = ref 1

  let make_empty = []

  let add_var (f:variable -> d_stmt list) (st:t) : (t * variable) =
    let count = !counter in
    counter := count + 1;
    let name = "_unknown_" ^ string_of_int count in
    let x = var_make name in
    (f x @ st, x)

  let add_stmt (s: d_stmt) (st:t) : t = s :: st

  let add_exp (source:d_exp) (ty:d_type) (st:t) : t * variable =
    add_var (fun x ->
      [
        DeclStmt [{name=x; ty=ty; init=Some (IExp source); attrs=[]}]
      ]
    ) st


  let add_write (a:d_subscript) (source:d_exp) (st:t) : (t * variable) =
    let wr x = WriteAccessStmt {target=a; source=VarDecl {name=x; ty=a.ty}} in
    match source with
    | VarDecl {name=x; _} ->
      (add_stmt (wr x) st, x)
    | _ ->
      add_var (fun x ->
        [
          wr x;
          DeclStmt [{name=x; ty=a.ty; init=Some (IExp source); attrs=[]}];
        ]
      ) st

  let add_read (a:d_subscript) (st:t) : (t * variable) =
    add_var (fun x ->
      [
        ReadAccessStmt {target=x; source=a};
      ]
    ) st
end

let rec rewrite_exp (c:Cast.c_exp) : (AccessState.t, d_exp) state =
  match c with
  | CXXOperatorCallExpr {
      func=CXXMethodDecl{name=v; _};
      args=[ArraySubscriptExpr a; src]
    } when var_name v = "operator="
    -> rewrite_write a src

  | BinaryOperator {lhs=ArraySubscriptExpr a; rhs=src; opcode="="; _} ->
    rewrite_write a src

  | ArraySubscriptExpr a -> rewrite_read a

  | BinaryOperator {lhs=l; rhs=r; opcode=o; ty=ty} ->
    fun st ->
    let (st, l) = rewrite_exp l st in
    let (st, r) = rewrite_exp r st in
    (st, BinaryOperator {lhs=l; rhs=r; opcode=o; ty=ty})

  | ConditionalOperator {cond=e1; then_expr=e2; else_expr=e3; ty=ty} ->
    fun st ->
    let (st, e1) = rewrite_exp e1 st in
    let (st, e2) = rewrite_exp e2 st in
    let (st, e3) = rewrite_exp e3 st in
    (st, ConditionalOperator {cond=e1; then_expr=e2; else_expr=e3; ty=ty})

  | CXXOperatorCallExpr {func=f; args=args; ty=ty} -> 
    fun st ->
    let (st, f) = rewrite_exp f st in
    let (st, args) = state_map rewrite_exp args st in
    (st, CXXOperatorCallExpr {func=f; args=args; ty=ty})

  | CallExpr {func=f; args=args; ty=ty} -> 
    fun st ->
    let (st, f) = rewrite_exp f st in
    let (st, args) = state_map rewrite_exp args st in
    (st, CallExpr {func=f; args=args; ty=ty})

  | CXXConstructExpr c ->
    fun st ->
    let (st, args) = state_map rewrite_exp c.args st in
    (st, CXXConstructExpr {args=args; ty=c.ty})

  | UnaryOperator {child=e; opcode=o; ty=ty} ->
    fun st ->
    let (st, e) = rewrite_exp e st in
    (st, UnaryOperator {child=e; opcode=o; ty=ty})

  | MemberExpr {base=e; name=o; ty=ty} ->
    fun st ->
    let (st, e) = rewrite_exp e st in
    state_pure (MemberExpr {base=e; name=o; ty=ty}) st

  | EnumConstantDecl v -> state_pure (EnumConstantDecl v)
  | VarDecl v -> state_pure (VarDecl v)
  | ParmVarDecl v -> state_pure (ParmVarDecl v)
  | FunctionDecl v -> state_pure (FunctionDecl v)
  | CXXMethodDecl v -> state_pure (CXXMethodDecl v)
  | NonTypeTemplateParmDecl v -> state_pure (NonTypeTemplateParmDecl v)
  | UnresolvedLookupExpr {name=n;tys=tys} -> state_pure (UnresolvedLookupExpr {name=n;tys=tys})
  | FloatingLiteral f -> state_pure (FloatingLiteral f)
  | IntegerLiteral i -> state_pure (IntegerLiteral i)
  | CharacterLiteral c -> state_pure (CharacterLiteral c)
  | CXXBoolLiteralExpr b -> state_pure (CXXBoolLiteralExpr b)

and rewrite_subscript (c:Cast.c_array_subscript) : (AccessState.t, d_subscript) state =
  let rec rewrite_subscript (c:Cast.c_array_subscript) (indices:d_exp list) : (AccessState.t, d_subscript) state =
    fun st ->
    let (st, idx) = rewrite_exp c.rhs st in
    let indices = idx :: indices in
    match c.lhs with
    | ArraySubscriptExpr a ->
      rewrite_subscript a indices st

    | VarDecl {name=n; ty=ty}
    | ParmVarDecl {name=n; ty=ty} ->
      state_pure {name=n; index=indices; ty=ty} st

    | e ->
      let ty = Cast.exp_type e in
      let (st, e) = rewrite_exp e st in
      let (st, x) = AccessState.add_exp e ty st in
      state_pure {name=x; index=indices; ty=ty} st
  in
  rewrite_subscript c []
and rewrite_write (a:Cast.c_array_subscript) (src:Cast.c_exp) : (AccessState.t, d_exp) state =
  fun st ->
    let (st, src') = rewrite_exp src st in
    let (st, a) = rewrite_subscript a st in
    let (st, x) = AccessState.add_write a src' st in
  state_pure (VarDecl {name=x; ty=Cast.exp_type src}) st
and rewrite_read (a:Cast.c_array_subscript): (AccessState.t, d_exp) state =
  fun st ->
    let (st, a) = rewrite_subscript a st in
    let (st, x) = AccessState.add_read a st in
    state_pure (VarDecl {name=x; ty=a.ty}) st

let map_opt (f:'a -> ('s * 'b)) (o:'a option) : ('s * 'b option) =
  match o with
  | Some v ->
    let (st, v) = f v in
    (st, Some v)
  | None -> ([], None)


let rewrite_exp (e:Cast.c_exp) : (d_stmt list * d_exp) =
  let (st, e) = rewrite_exp e AccessState.make_empty in
  (st |> List.rev, e)

let rewrite_exp_list (es:Cast.c_exp list) : (d_stmt list * d_exp list) =
  let (ss, es) = List.map rewrite_exp es |> List.split in
  (List.concat ss, es)

let rewrite_decl (d:Cast.c_decl) : (d_stmt list * d_decl) =
  let rewrite_init (c:Cast.c_init) : (d_stmt list * d_init) =
    match c with
    | InitListExpr {ty=ty; args=args} ->
      let (pre, args) = rewrite_exp_list args in
      (pre, InitListExpr {ty=ty; args=args})
    | IExp e ->
      let (pre, e) = rewrite_exp e in
      (pre, IExp e)
  in
  let (pre, o) = map_opt rewrite_init d.init in
  (pre, {name=d.name; ty=d.ty; init=o; attrs=d.attrs})

let rewrite_for_init (f:Cast.c_for_init) : (d_stmt list * d_for_init) =
  match f with
  | ForDecl d ->
    let (pre, d) = List.map rewrite_decl d |> List.split in
    (List.concat pre, ForDecl d)
  | ForExp e ->
    let (s, e) = rewrite_exp e in
    (s, ForExp e)

let rec rewrite_stmt (s:Cast.c_stmt) : d_stmt =
  let block (pre:d_stmt list) (s:d_stmt) =
    match pre with
    | [] -> s
    | _ -> CompoundStmt (pre @ [s])
  in
  match s with
  | BreakStmt -> BreakStmt
  | GotoStmt -> GotoStmt
  | ReturnStmt -> ReturnStmt
  | IfStmt {cond=c; then_stmt=s1; else_stmt=s2} ->
    let (pre, c) = rewrite_exp c in
    block pre (IfStmt {cond=c; then_stmt=rewrite_stmt s1; else_stmt=rewrite_stmt s2})
  | CompoundStmt l -> CompoundStmt (List.map rewrite_stmt l)

  | DeclStmt d ->
    let (pre, d) = List.map rewrite_decl d |> List.split in
    block (List.concat pre) (DeclStmt d)

  | WhileStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    block pre (WhileStmt {cond=c; body=rewrite_stmt b})

  | ForStmt {init=e1; cond=e2; inc=e3; body=b} ->
    let (pre1, e1) = map_opt rewrite_for_init e1 in
    let (pre2, e2) = map_opt rewrite_exp e2 in
    let (pre3, e3) = map_opt rewrite_exp e3 in
    block (pre1 @ pre2 @ pre3) (ForStmt {init=e1; cond=e2; inc=e3; body=rewrite_stmt b})

  | DoStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    block pre (DoStmt {cond=c; body=rewrite_stmt b})

  | SwitchStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    block pre (SwitchStmt {cond=c; body=rewrite_stmt b})

  | CaseStmt {case=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    block pre (CaseStmt {case=c; body=rewrite_stmt b})
  | DefaultStmt s ->
    DefaultStmt (rewrite_stmt s)
  | SExp e ->
    let (pre, e) = rewrite_exp e in
    block pre (SExp e)

let rewrite_kernel (k:Cast.c_kernel) : d_kernel =
  {
    name = k.name;
    code = rewrite_stmt k.code;
    params = k.params;
  }

let rewrite_def (d:Cast.c_def) : d_def =
  match d with
  | Kernel k -> Kernel (rewrite_kernel k)
  | Declaration d -> Declaration d

let rewrite_program: Cast.c_program -> d_program =
  List.map rewrite_def

(* ------------------------------------------------------------------------ *)

let list_to_s (f:'a -> string) (l:'a list) : string =
  List.map f l |> Common.join ", "

let rec exp_to_s : d_exp -> string =
  let type_to_str = Cast.type_to_str in
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
  | CXXBoolLiteralExpr b -> if b then "true" else "false";
  | CXXConstructExpr c -> "@ctor " ^ type_to_str c.ty ^ "(" ^ list_to_s exp_to_s c.args ^ ")" 
  | CXXMethodDecl v -> "@meth " ^ var_name v.name
  | CXXOperatorCallExpr c -> exp_to_s c.func ^ "!(" ^ list_to_s exp_to_s c.args  ^ ")"
  | CallExpr c -> exp_to_s c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
  | VarDecl v -> var_name v.name
  | UnresolvedLookupExpr v -> "@unresolv " ^ var_name v.name
  | NonTypeTemplateParmDecl v -> "@tpl " ^ var_name v.name
  | FunctionDecl v -> "@func " ^ var_name v.name
  | ParmVarDecl v -> "@parm " ^ var_name v.name
  | EnumConstantDecl v -> "@enum " ^ var_name v.name
  | UnaryOperator u -> u.opcode ^ exp_to_s u.child

let init_to_s : d_init -> string =
  function
  | CXXConstructExpr c -> "ctor"
  | InitListExpr i -> list_to_s exp_to_s i.args
  | IExp i -> exp_to_s i

let decl_to_s (d: d_decl): string =
  let i = match d.init with
    | Some e -> " = " ^ init_to_s e
    | None -> ""
  in
  var_name d.name ^ i

let subscript_to_s (s:d_subscript) : string =
  var_name s.name ^ "[" ^ list_to_s exp_to_s s.index ^ "]"

let for_init_to_s (f:d_for_init) : string =
  match f with
  | ForDecl d -> list_to_s decl_to_s d
  | ForExp e -> exp_to_s e

let opt_for_init_to_s (o:d_for_init option) : string =
  match o with
  | Some o -> for_init_to_s o
  | None -> ""

let stmt_to_s: d_stmt -> PPrint.t list =
  let opt_exp_to_s: d_exp option -> string =
    function
    | Some c -> exp_to_s c
    | None -> ""
  in
  let open PPrint in
  let rec stmt_to_s : d_stmt -> PPrint.t list =
    function
    | WriteAccessStmt w -> [Line ("rw " ^ subscript_to_s w.target ^ " = " ^ exp_to_s w.source ^ ";")]
    | ReadAccessStmt r -> [Line ("ro " ^ var_name r.target ^ " = " ^ subscript_to_s r.source ^ ";")]
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

let summarize_stmt: d_stmt -> string =
  let opt_exp_to_s: d_exp option -> string =
    function
    | Some c -> exp_to_s c
    | None -> ""
  in
  let rec stmt_to_s : d_stmt -> string =
    function
    | WriteAccessStmt w ->
      "rw " ^
      subscript_to_s w.target ^
      " = " ^
      exp_to_s w.source ^ ";"
    | ReadAccessStmt r -> "ro " ^ var_name r.target ^ " = " ^ subscript_to_s r.source ^ ";"
    | ReturnStmt -> "return;"
    | GotoStmt -> "goto;"
    | BreakStmt -> "break;"
    | ForStmt f ->
        "for (" ^
        opt_for_init_to_s f.init ^ "; " ^
        opt_exp_to_s f.cond ^ "; " ^
        opt_exp_to_s f.inc ^
        ") {...}"
    | WhileStmt {cond=b; body=s} -> "while (" ^ exp_to_s b ^ ") {...}"
    | DoStmt {cond=b; body=s} -> "{...} do (" ^ exp_to_s b ^ ")";
    | SwitchStmt {cond=b; body=s} -> "switch (" ^ exp_to_s b ^ ") {...}";
    | CaseStmt c -> "case " ^ exp_to_s c.case ^ ": {...}"
    | DefaultStmt d -> "default: {...}"
    | IfStmt {cond=b; then_stmt=s1; else_stmt=s2} ->
      "if (" ^ exp_to_s b ^ ") {...} else {...}"
    | CompoundStmt l ->
      let c = List.length l |> string_of_int in
      "{ " ^ c ^ " stmts... }"
    | DeclStmt d ->
      "decl {" ^ Common.join ", " (List.map decl_to_s d) ^ "}"
    | SExp e -> exp_to_s e
  in
  stmt_to_s

let kernel_to_s (k:d_kernel) : PPrint.t list =
  let open PPrint in
  [
    Line ("name: " ^ k.name);
    Line ("params: " ^ list_to_s Cast.param_to_s k.params);
  ]
  @
  stmt_to_s k.code

let def_to_s (d:d_def) : PPrint.t list =
  let open PPrint in
  match d with
  | Declaration d -> [Line (Cast.type_to_str d.ty ^ " " ^ var_name d.name ^ ";")]
  | Kernel k -> kernel_to_s k

let program_to_s (p:d_program) : PPrint.t list =
  List.concat_map def_to_s p

let print_program (p:d_program) : unit =
  PPrint.print_doc (program_to_s p)

