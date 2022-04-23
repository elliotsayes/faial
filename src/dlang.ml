module StackTrace = Common.StackTrace

open Exp
open Serialize
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

type d_type = json

type d_exp =
  | CharacterLiteral of int
  | BinaryOperator of d_binary
  | CallExpr of {func: d_exp; args: d_exp list}
  | ConditionalOperator of {cond: d_exp; then_expr: d_exp; else_expr: d_exp; ty: d_type}
  | CXXBoolLiteralExpr of bool
  | CXXMethodDecl of {name: variable; ty: d_type}
  | CXXOperatorCallExpr of {func: d_exp; args: d_exp list}
  | FloatingLiteral of float
  | FunctionDecl of {name: variable; ty: d_type}
  | IntegerLiteral of int
  | NonTypeTemplateParmDecl of {name: variable; ty: d_type}
  | MemberExpr of {name: string; base: d_exp}
  | ParmVarDecl of {name: variable; ty: d_type}
  | DeclRefExpr of d_type
  | PredicateExpr of {child: d_exp; opcode: string}
  | UnaryOperator of { opcode: string; child: d_exp; ty: d_type}
  | VarDecl of {name: variable; ty: d_type}
  | UnresolvedLookupExpr of {name: variable; tys: d_type list}
and d_binary = {opcode: string; lhs: d_exp; rhs: d_exp; ty: d_type}

let exp_name = function
| CharacterLiteral _ -> "CharacterLiteral"
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


type d_init =
  | CXXConstructExpr of {constructor: d_type; ty: d_type}
  | InitListExpr of {ty: d_type; args: d_exp list}
  | IExp of d_exp
  

type d_range = {
  name: variable;
  lower_bound: d_exp;
  upper_bound: d_exp;
  step: d_exp;
  opcode: string
}

type d_decl = {
  name: variable;
  ty: d_type;
  init: d_init option;
  attrs: string list
}

type d_subscript = {name: variable; index: d_exp list; ty: d_type}

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
  | ForStmt of {init: d_exp option; cond: d_exp option; inc: d_exp option; body: d_stmt}
  | DoStmt of {cond: d_exp; body: d_stmt}
  | SwitchStmt of {cond: d_exp; body: d_stmt}
  | DefaultStmt of d_stmt
  | CaseStmt of {case: d_exp; body: d_stmt}
  | SyncStmt (* faial-infer *)
  | ForEachStmt of {range: d_range; body: d_stmt} (* faial-infer *)
  | AssertStmt of d_exp (* faial-infer *)
  | SExp of d_exp
  and d_write = {target: d_subscript; source: d_exp}
  and d_read = {target: variable; source: d_subscript}

type d_kernel = {
  name: string;
  code: d_stmt;
}

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
  type t = {known: VarSet.t; side_effects: d_stmt list}

  let make_empty =
    {known=VarSet.empty; side_effects=[]}

  let add_var (f:variable -> d_stmt list) (st:t) : (t * variable) =
    let name = "_unknown_" ^ (string_of_int (VarSet.cardinal st.known)) in
    let x = Bindings.generate_fresh_name (var_make name) st.known in
    ({known = VarSet.add x st.known; side_effects = f x @ st.side_effects}, x)

  let add_exp (source:d_exp) (ty:d_type) (st:t) : t * variable =
    add_var (fun x ->
      [
        DeclStmt [{name=x; ty=ty; init=Some (IExp source); attrs=[]}]
      ]
    ) st


  let add_write (a:d_subscript) (source:d_exp) (st:t) : (t * variable) =
    add_var (fun x ->
      [
        DeclStmt [{name=x; ty=a.ty; init=Some (IExp source); attrs=[]}];
        WriteAccessStmt {target=a; source=VarDecl {name=x; ty=a.ty}};
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

  | CXXOperatorCallExpr {func=f; args=args} -> 
    fun st ->
    let (st, f) = rewrite_exp f st in
    let (st, args) = state_map rewrite_exp args st in
    (st, CXXOperatorCallExpr {func=f; args=args})
  | CallExpr {func=f; args=args} -> 
    fun st ->
    let (st, f) = rewrite_exp f st in
    let (st, args) = state_map rewrite_exp args st in
    (st, CallExpr {func=f; args=args})
  | UnaryOperator {child=e; opcode=o; ty=ty} ->
    fun st ->
    let (st, e) = rewrite_exp e st in
    (st, UnaryOperator {child=e; opcode=o; ty=ty})
  | PredicateExpr {child=e; opcode=o} ->
    fun st ->
    let (st, e) = rewrite_exp e st in
    state_pure (PredicateExpr {child=e; opcode=o}) st
  | MemberExpr {base=e; name=o} ->
    fun st ->
    let (st, e) = rewrite_exp e st in
    state_pure (MemberExpr {base=e; name=o}) st
  | VarDecl {name=n;ty=ty} -> state_pure (VarDecl {name=n;ty=ty})
  | ParmVarDecl {name=n;ty=ty} -> state_pure (ParmVarDecl {name=n;ty=ty})
  | FunctionDecl {name=n;ty=ty} -> state_pure (FunctionDecl {name=n;ty=ty})
  | CXXMethodDecl {name=n;ty=ty} -> state_pure (CXXMethodDecl {name=n;ty=ty})
  | NonTypeTemplateParmDecl {name=n;ty=ty} -> state_pure (NonTypeTemplateParmDecl {name=n;ty=ty})
  | UnresolvedLookupExpr {name=n;tys=tys} -> state_pure (UnresolvedLookupExpr {name=n;tys=tys})
  | FloatingLiteral f -> state_pure (FloatingLiteral f)
  | IntegerLiteral i -> state_pure (IntegerLiteral i)
  | CharacterLiteral c -> state_pure (CharacterLiteral c)
  | DeclRefExpr c -> state_pure (DeclRefExpr c)
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

let rec rewrite_stmt (s:Cast.c_stmt) : d_stmt =
  let block (pre:d_stmt list) (s:d_stmt) =
    match pre with
    | [] -> s
    | _ -> CompoundStmt (pre @ [s])
  in
  let rewrite_exp (e:Cast.c_exp) : (d_stmt list * d_exp) =
    let (st, e) = rewrite_exp e AccessState.make_empty in
    (st.side_effects, e)
  in
  let rewrite_exp_list (es:Cast.c_exp list) : (d_stmt list * d_exp list) =
    let (ss, es) = List.map rewrite_exp es |> List.split in
    (List.concat ss, es)
  in
  match s with
  | BreakStmt -> BreakStmt
  | GotoStmt -> GotoStmt
  | ReturnStmt -> ReturnStmt
  | SyncStmt -> SyncStmt
  | IfStmt {cond=c; then_stmt=s1; else_stmt=s2} ->
    let (pre, c) = rewrite_exp c in
    block pre (IfStmt {cond=c; then_stmt=rewrite_stmt s1; else_stmt=rewrite_stmt s2})
  | CompoundStmt l -> CompoundStmt (List.map rewrite_stmt l)

  | DeclStmt d ->
    let rewrite_decl (d:Cast.c_decl) : (d_stmt list * d_decl) =
      let rewrite_init (c:Cast.c_init) : (d_stmt list * d_init) =
        match c with
        | CXXConstructExpr {constructor=c1; ty=ty} ->
          ([], CXXConstructExpr {constructor=c1; ty=ty})
        | InitListExpr {ty=ty; args=args} ->
          let (pre, args) = rewrite_exp_list args in
          (pre, InitListExpr {ty=ty; args=args})
        | IExp e ->
          let (pre, e) = rewrite_exp e in
          (pre, IExp e)
      in
      let (pre, o) = map_opt rewrite_init d.init in
      (pre, {name=d.name; ty=d.ty; init=o; attrs=d.attrs})
    in
    let (pre, d) = List.map rewrite_decl d |> List.split in
    block (List.concat pre) (DeclStmt d)

  | WhileStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    block pre (WhileStmt {cond=c; body=rewrite_stmt b})

  | ForStmt {init=e1; cond=e2; inc=e3; body=b} ->
    let (pre1, e1) = map_opt rewrite_exp e1 in
    let (pre2, e2) = map_opt rewrite_exp e2 in
    let (pre3, e3) = map_opt rewrite_exp e3 in
    block (pre1 @ pre2 @ pre3) (ForStmt {init=e1; cond=e2; inc=e3; body=rewrite_stmt b})

  | ForEachStmt {range=r; body=b} ->
    let rewrite_range (r:Cast.c_range) : (d_stmt list * d_range)  =
      let (pre1, l) = rewrite_exp r.lower_bound in
      let (pre2, u) = rewrite_exp r.upper_bound in
      let (pre3, s) = rewrite_exp r.step in
      (pre1 @ pre2 @ pre3, {name=r.name; lower_bound=l; upper_bound=u; step=s; opcode=r.opcode})
    in
    let (pre, r) = rewrite_range r in
    block pre (ForEachStmt {range=r; body=rewrite_stmt b})

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
  | AssertStmt e ->
    let (pre, e) = rewrite_exp e in
    block pre (AssertStmt e)



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


let range_to_s (r:d_range) : string =
  exp_to_s r.lower_bound ^ " .. " ^ exp_to_s r.upper_bound ^ "; " ^ r.opcode ^ exp_to_s r.step

let subscript_to_s (s:d_subscript) : string =
  var_name s.name ^ "[" ^ list_to_s exp_to_s s.index ^ "]"

let stmt_to_s: d_stmt -> PPrint.t list =
  let opt_exp_to_s: d_exp option -> string =
    function
    | Some c -> exp_to_s c
    | None -> ""
  in
  let open PPrint in
  let rec stmt_to_s : d_stmt -> PPrint.t list =
    function
    | WriteAccessStmt w -> [Line ("wr " ^ subscript_to_s w.target ^ " = " ^ exp_to_s w.source ^ ";")]
    | ReadAccessStmt r -> [Line ("rd " ^ var_name r.target ^ " = " ^ subscript_to_s r.source ^ ";")]
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

let kernel_to_s (k:d_kernel) : PPrint.t list =
  let open PPrint in
  stmt_to_s k.code

let print_kernel (k: d_kernel) : unit =
  PPrint.print_doc (kernel_to_s k)

let print_stmt s =
  PPrint.print_doc (stmt_to_s s)

