open Stage0
open Stage1

module StackTrace = Common.StackTrace
module KernelAttr = C_lang.KernelAttr

open Serialize
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

type array_t = Stage1.Exp.array_t
type d_type = json
type d_var = C_lang.c_var

let list_to_s (f:'a -> string) (l:'a list) : string =
  List.map f l |> Common.join ", "

module Expr = struct
  type t =
    | SizeOfExpr of d_type
    | CXXNewExpr of {arg: t; ty: d_type}
    | CXXDeleteExpr of {arg: t; ty: d_type}
    | RecoveryExpr of d_type
    | CharacterLiteral of int
    | BinaryOperator of d_binary
    | CallExpr of {func: t; args: t list; ty: d_type}
    | ConditionalOperator of {cond: t; then_expr: t; else_expr: t; ty: d_type}
    | CXXConstructExpr of {args: t list; ty: d_type}
    | CXXBoolLiteralExpr of bool
    | CXXMethodDecl of d_var
    | CXXOperatorCallExpr of {func: t; args: t list; ty: d_type}
    | FloatingLiteral of float
    | FunctionDecl of d_var
    | IntegerLiteral of int
    | NonTypeTemplateParmDecl of d_var
    | MemberExpr of {name: string; base: t; ty: d_type}
    | ParmVarDecl of d_var
    | UnaryOperator of { opcode: string; child: t; ty: d_type}
    | VarDecl of d_var
    | EnumConstantDecl of d_var
    | UnresolvedLookupExpr of {name: Variable.t; tys: d_type list}
  and d_binary = {opcode: string; lhs: t; rhs: t; ty: d_type}

  let to_variable : t -> Variable.t option =
    function
    | CXXMethodDecl {name=n}
    | FunctionDecl {name=n}
    | NonTypeTemplateParmDecl {name=n}
    | ParmVarDecl {name=n}
    | VarDecl {name=n}
    | UnresolvedLookupExpr {name=n} -> Some n
    | _ -> None

  let name =
    function
    | SizeOfExpr _ -> "SizeOfExpr"
    | CXXNewExpr _ -> "CXXNewExpr"
    | CXXDeleteExpr _ -> "CXXNewExpr"
    | RecoveryExpr _ -> "RecoveryExpr"
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

  let rec to_type :  t -> d_type =
    function
    | SizeOfExpr c -> C_type.j_int_type
    | CXXNewExpr c -> c.ty
    | CXXDeleteExpr c -> c.ty
    | RecoveryExpr ty -> ty
    | CharacterLiteral _ -> C_type.j_char_type
    | BinaryOperator a -> a.ty
    | ConditionalOperator c -> to_type c.then_expr
    | CXXBoolLiteralExpr _ -> C_type.j_bool_type
    | CXXMethodDecl a -> a.ty
    | CXXConstructExpr c -> c.ty
    | FloatingLiteral _ -> C_type.j_float_type
    | FunctionDecl a -> a.ty
    | IntegerLiteral _ -> C_type.j_int_type
    | NonTypeTemplateParmDecl a -> a.ty
    | ParmVarDecl a -> a.ty
    | UnaryOperator a -> a.ty
    | VarDecl a -> a.ty
    | CallExpr c -> c.ty
    | CXXOperatorCallExpr a -> a.ty
    | MemberExpr a -> a.ty
    | EnumConstantDecl a -> a.ty
    | UnresolvedLookupExpr a -> C_type.mk_j_type "?"


  let to_string ?(modifier:bool=false) ?(provenance:bool=false) ?(types:bool=false) : t -> string =
    let attr (s:string) : string =
      if modifier
      then "@" ^ s ^ " "
      else ""
    in
    let opcode (o:string) (j:Yojson.Basic.t) : string =
      if types
      then "(" ^ o ^ "." ^ C_lang.type_to_str j ^ ")"
      else o
    in
    let var_name: Variable.t -> string =
      if provenance
      then Variable.repr
      else Variable.name
    in
    let rec exp_to_s: t -> string =
      let par (e: t) : string =
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
        | MemberExpr _
        | IntegerLiteral _
        | CharacterLiteral _
        | RecoveryExpr _
        | FloatingLiteral _
        | SizeOfExpr _
          ->  exp_to_s e
      in
      function
      | SizeOfExpr ty -> "sizeof(" ^ C_lang.type_to_str ty ^ ")"
      | CXXNewExpr c -> "new " ^ C_lang.type_to_str c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
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
      | CXXBoolLiteralExpr b -> if b then "true" else "false";
      | CXXConstructExpr c -> attr "ctor" ^ C_lang.type_to_str c.ty ^ "(" ^ list_to_s exp_to_s c.args ^ ")"
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

  let opt_to_string : t option -> string =
    function
    | Some c -> to_string c
    | None -> ""

end

module Init = struct
  type t =
    | CXXConstructExpr of {constructor: d_type; ty: d_type}
    | InitListExpr of {ty: d_type; args: Expr.t list}
    | IExpr of Expr.t

  let to_exp (i:t) : Expr.t list =
    match i with
    | CXXConstructExpr _ -> []
    | InitListExpr i -> i.args
    | IExpr e -> [e]



  let to_string : t -> string =
    function
    | CXXConstructExpr c -> "ctor"
    | InitListExpr i -> list_to_s Expr.to_string i.args
    | IExpr i -> Expr.to_string i

end

module Decl = struct
  type t = {
    name: Variable.t;
    ty: d_type;
    init: Init.t option;
    attrs: string list
  }

  let get_shared (d:t) : array_t option =
    if List.mem C_lang.c_attr_shared d.attrs
    then match C_lang.parse_type d.ty with
      | Ok ty ->
        Some {
          array_hierarchy = SharedMemory;
          array_size = C_type.get_array_length ty;
          array_type = C_type.get_array_type ty;
        }
      | Error _ -> None
    else None


  let to_exp (d:t) : Expr.t list =
    match d.init with
    | Some i -> Init.to_exp i
    | None -> []

  let to_string (d: t) : string =
    let i = match d.init with
      | Some e -> " = " ^ Init.to_string e
      | None -> ""
    in
    let attr = if d.attrs = [] then "" else
      let attrs = Common.join " " d.attrs |> String.trim in
      attrs ^ " "
    in
    attr ^ C_lang.type_to_str d.ty ^ " " ^ Variable.name d.name ^ i

end

type d_decl = Decl.t


module ForInit = struct
  type t =
    | ForDecl of d_decl list
    | ForExpr of Expr.t

  let to_exp (f:t) : Expr.t list =
    match f with
    | ForDecl l -> List.fold_left
      (fun l d -> Common.append_rev1 (Decl.to_exp d) l)
      []
      l
    | ForExpr e -> [e]

  (* Returns the binders of a for statement *)
  let loop_vars : t -> Variable.t list =
    let rec exp_var (e:Expr.t) : Variable.t list =
      match e with
      | BinaryOperator {lhs=l; opcode=","; rhs=r} ->
        exp_var l |> Common.append_rev1 (exp_var r)
      | BinaryOperator {lhs=l; opcode="="; rhs=r} ->
        (match Expr.to_variable l with
        | Some x -> [x]
        | None -> [])
      | _ -> []
    in
    function
    | ForDecl l -> List.map (fun (d:d_decl) -> d.name) l
    | ForExpr e -> exp_var e

  let to_string : t -> string =
    function
    | ForDecl d -> list_to_s Decl.to_string d
    | ForExpr e -> Expr.to_string e


  let opt_to_string (o:t option) : string =
    o
    |> Option.map to_string
    |> Ojson.unwrap_or ""

end

type d_subscript = {name: Variable.t; index: Expr.t list; ty: d_type; location: Location.t}
type d_write = {target: d_subscript; source: Expr.t}
type d_read = {target: Variable.t; source: d_subscript}

type d_stmt =
  | WriteAccessStmt of d_write
  | ReadAccessStmt of d_read
  | BreakStmt
  | GotoStmt
  | ReturnStmt
  | ContinueStmt
  | IfStmt of {cond: Expr.t; then_stmt: d_stmt; else_stmt: d_stmt}
  | CompoundStmt of d_stmt list
  | DeclStmt of d_decl list
  | WhileStmt of {cond: Expr.t; body: d_stmt}
  | ForStmt of d_for
  | DoStmt of {cond: Expr.t; body: d_stmt}
  | SwitchStmt of {cond: Expr.t; body: d_stmt}
  | DefaultStmt of d_stmt
  | CaseStmt of {case: Expr.t; body: d_stmt}
  | SExpr of Expr.t
and d_for = {init: ForInit.t option; cond: Expr.t option; inc: Expr.t option; body: d_stmt}


type d_kernel = {
  name: string;
  code: d_stmt;
  type_params: C_lang.c_type_param list;
  params: C_lang.c_param list;
  attribute: KernelAttr.t;
}

type d_def =
  | Kernel of d_kernel
  | Declaration of d_decl

type d_program = d_def list


(* ------------------------------------- *)

let for_to_expr (f:d_for) : Expr.t list =
  let l1 = f.init |> Option.map ForInit.to_exp |> Ojson.unwrap_or [] in
  let l2 = f.cond |> Option.map (fun x -> [x]) |> Ojson.unwrap_or [] in
  let l3 = f.inc |> Option.map (fun x -> [x]) |> Ojson.unwrap_or [] in
  l1
  |> Common.append_rev1 l2
  |> Common.append_rev1 l3

let for_loop_vars (f:d_for) : Variable.t list =
  f.init
  |> Option.map ForInit.loop_vars
  |> Ojson.unwrap_or []

(* ------------------------------------- *)


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
let (@) = Common.append_tr

module AccessState = struct
  type t = d_stmt list

  let counter = ref 1

  let make_empty = []

  let add_var (f:Variable.t -> d_stmt list) (st:t) : (t * Variable.t) =
    let count = !counter in
    counter := count + 1;
    let name = "_unknown_" ^ string_of_int count in
    let x = Variable.from_name name in
    (f x @ st, x)

  let add_stmt (s: d_stmt) (st:t) : t = s :: st

  let add_expr (source:Expr.t) (ty:d_type) (st:t) : t * Variable.t =
    add_var (fun x ->
      [
        DeclStmt [{name=x; ty=ty; init=Some (IExpr source); attrs=[]}]
      ]
    ) st


  let add_write (a:d_subscript) (source:Expr.t) (st:t) : (t * Variable.t) =
    let wr x = WriteAccessStmt {target=a; source=VarDecl {name=x; ty=a.ty}} in
    match source with
    | VarDecl {name=x; _} ->
      (add_stmt (wr x) st, x)
    | _ ->
      add_var (fun x ->
        [
          wr x;
          DeclStmt [{name=x; ty=a.ty; init=Some (IExpr source); attrs=[]}];
        ]
      ) st

  let add_read (a:d_subscript) (st:t) : (t * Variable.t) =
    add_var (fun x ->
      [
        ReadAccessStmt {target=x; source=a};
      ]
    ) st
end

let rec rewrite_exp (c:C_lang.Expr.t) : (AccessState.t, Expr.t) state =
  let open Expr in
  match c with
  | CXXOperatorCallExpr {
      func=CXXMethodDecl{name=v; _};
      args=[ArraySubscriptExpr a; src]
    } when Variable.name v = "operator="
    -> rewrite_write a src

  | SizeOfExpr ty ->
    fun st -> (st, RecoveryExpr ty)

  | RecoveryExpr ty ->
    fun st -> (st, RecoveryExpr ty)

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

  | CXXNewExpr {arg=arg; ty=ty} -> 
    fun st ->
    let (st, arg) = rewrite_exp arg st in
    (st, CXXNewExpr {arg=arg; ty=ty})

  | CXXDeleteExpr {arg=arg; ty=ty} -> 
    fun st ->
    let (st, arg) = rewrite_exp arg st in
    (st, CXXDeleteExpr {arg=arg; ty=ty})

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

  | UnaryOperator {child=ArraySubscriptExpr a; opcode="&"; ty=ty} ->
    rewrite_exp (BinaryOperator{lhs=a.lhs; opcode="+"; rhs=a.rhs; ty=ty})

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

and rewrite_subscript (c:C_lang.Expr.c_array_subscript) : (AccessState.t, d_subscript) state =
  let rec rewrite_subscript (c:C_lang.Expr.c_array_subscript) (indices:Expr.t list) (loc:Location.t option) : (AccessState.t, d_subscript) state =
    fun st ->
    let (st, idx) = rewrite_exp c.rhs st in
    let loc = Some (match loc with
    | Some loc -> Location.add_or_lhs loc c.location
    | None -> c.location)
    in
    let indices = idx :: indices in
    match c.lhs with
    | ArraySubscriptExpr a ->
      rewrite_subscript a indices loc st

    | VarDecl {name=n; ty=ty}
    | ParmVarDecl {name=n; ty=ty} ->
      state_pure {name=n; index=indices; ty=ty; location=Option.get loc} st

    | e ->
      let ty = C_lang.Expr.to_type e in
      let (st, e) = rewrite_exp e st in
      let (st, x) = AccessState.add_expr e ty st in
      state_pure {name=x; index=indices; ty=ty; location=Option.get loc} st
  in
  rewrite_subscript c [] None
and rewrite_write (a:C_lang.Expr.c_array_subscript) (src:C_lang.Expr.t) : (AccessState.t, Expr.t) state =
  fun st ->
    let (st, src') = rewrite_exp src st in
    let (st, a) = rewrite_subscript a st in
    let (st, x) = AccessState.add_write a src' st in
  state_pure (Expr.VarDecl {name=x; ty=C_lang.Expr.to_type src}) st
and rewrite_read (a:C_lang.Expr.c_array_subscript): (AccessState.t, Expr.t) state =
  fun st ->
    let (st, a) = rewrite_subscript a st in
    let (st, x) = AccessState.add_read a st in
    state_pure (Expr.VarDecl {name=x; ty=a.ty}) st

let map_opt (f:'a -> ('s * 'b)) (o:'a option) : ('s * 'b option) =
  match o with
  | Some v ->
    let (st, v) = f v in
    (st, Some v)
  | None -> ([], None)


let rewrite_exp (e:C_lang.Expr.t) : (d_stmt list * Expr.t) =
  let (st, e) = rewrite_exp e AccessState.make_empty in
  (st |> List.rev, e)

let rewrite_exp_list (es:C_lang.Expr.t list) : (d_stmt list * Expr.t list) =
  let (ss, es) = List.map rewrite_exp es |> List.split in
  (List.concat ss, es)

let rewrite_decl (d:C_lang.Decl.t) : (d_stmt list * Decl.t) =
  let rewrite_init (c:C_lang.Init.t) : (d_stmt list * Init.t) =
    match c with
    | InitListExpr {ty=ty; args=args} ->
      let (pre, args) = rewrite_exp_list args in
      (pre, InitListExpr {ty=ty; args=args})
    | IExpr e ->
      let (pre, e) = rewrite_exp e in
      (pre, IExpr e)
  in
  let (pre, o) = map_opt rewrite_init d.init in
  (pre, {name=d.name; ty=d.ty; init=o; attrs=d.attrs})

let rewrite_for_init (f:C_lang.ForInit.t) : (d_stmt list * ForInit.t) =
  match f with
  | ForDecl d ->
    let (pre, d) = List.map rewrite_decl d |> List.split in
    (List.concat pre, ForDecl d)
  | ForExpr e ->
    let (s, e) = rewrite_exp e in
    (s, ForExpr e)

let rec rewrite_stmt (s:C_lang.c_stmt) : d_stmt list =
  let decl (pre:d_stmt list) (s:d_stmt) =
    match pre with
    | [] -> [s]
    | _ -> [CompoundStmt (pre @ [s])]
  in
  let rewrite_s (s:C_lang.c_stmt) : d_stmt =
    match rewrite_stmt s with
    | [s] -> s
    | l -> CompoundStmt l
  in
  match s with
  | BreakStmt -> [BreakStmt]
  | GotoStmt -> [GotoStmt]
  | ReturnStmt -> [ReturnStmt]
  | ContinueStmt -> [ContinueStmt]
  | IfStmt {cond=c; then_stmt=s1; else_stmt=s2} ->
    let (pre, c) = rewrite_exp c in
    decl pre (IfStmt {
      cond=c;
      then_stmt=rewrite_s s1;
      else_stmt=rewrite_s s2;
    })
  | CompoundStmt l -> [CompoundStmt (List.concat_map rewrite_stmt l)]

  | DeclStmt d ->
    let (pre, d) = List.map rewrite_decl d |> List.split in
    List.concat pre @ [DeclStmt d]

  | WhileStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    decl pre (WhileStmt {cond=c; body=rewrite_s b})

  | ForStmt {init=e1; cond=e2; inc=e3; body=b} ->
    let (pre1, e1) = map_opt rewrite_for_init e1 in
    let (pre2, e2) = map_opt rewrite_exp e2 in
    let (pre3, e3) = map_opt rewrite_exp e3 in
    decl (pre1 @ pre2 @ pre3) (
      ForStmt {init=e1; cond=e2; inc=e3; body=rewrite_s b}
    )

  | DoStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    decl pre (DoStmt {cond=c; body=rewrite_s b})

  | SwitchStmt {cond=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    decl pre (SwitchStmt {cond=c; body=rewrite_s b})

  | CaseStmt {case=c; body=b} ->
    let (pre, c) = rewrite_exp c in
    decl pre (CaseStmt {case=c; body=rewrite_s b})
  | DefaultStmt s ->
    [DefaultStmt (rewrite_s s)]
  | SExpr e ->
    let (pre, e) = rewrite_exp e in
    decl pre (SExpr e)

let rewrite_kernel (k:C_lang.c_kernel) : d_kernel =
  let rewrite_s (s:C_lang.c_stmt) : d_stmt =
    match rewrite_stmt s with
    | [s] -> s
    | l -> CompoundStmt l
  in
  {
    name = k.name;
    code = rewrite_s k.code;
    params = k.params;
    type_params = k.type_params;
    attribute = k.attribute;
  }

let rewrite_def (d:C_lang.c_def) : d_def =
  match d with
  | Kernel k -> Kernel (rewrite_kernel k)
  | Declaration d ->
    let (pre, d) = rewrite_decl d in
    Declaration d

let rewrite_program: C_lang.c_program -> d_program =
  List.map rewrite_def

(* ------------------------------------------------------------------------ *)

let subscript_to_s (s:d_subscript) : string =
  Variable.name s.name ^ "[" ^ list_to_s Expr.to_string s.index ^ "]"

let stmt_to_s: d_stmt -> PPrint.t list =
  let open PPrint in
  let rec stmt_to_s : d_stmt -> PPrint.t list =
    let ret l : PPrint.t list =
      match l with
      | [] -> [Line ";"]
      | [Line "{"; Block l; Line "}"]
      | l -> [Line "{"; Block l; Line "}"]
    in
    let block (s:d_stmt) : PPrint.t list = ret (stmt_to_s s) in 
    function
    | WriteAccessStmt w -> [Line ("rw " ^ subscript_to_s w.target ^ " = " ^ Expr.to_string w.source)]
    | ReadAccessStmt r -> [Line ("ro " ^ Variable.name r.target ^ " = " ^ subscript_to_s r.source)]
    | ReturnStmt -> [Line "return"]
    | GotoStmt -> [Line "goto"]
    | ContinueStmt -> [Line "continue"]
    | BreakStmt -> [Line "break"]
    | ForStmt f -> [
        Line ("for (" ^ ForInit.opt_to_string f.init ^ "; " ^ Expr.opt_to_string f.cond ^ "; " ^ Expr.opt_to_string f.inc ^ ")");
      ]
      @ block (f.body)
    | WhileStmt {cond=b; body=s} ->
        [ Line ("while (" ^ Expr.to_string b ^ ")"); ] @
        block s
    | DoStmt {cond=b; body=s} ->
      block s @ [ Line ("do (" ^ Expr.to_string b ^ ")"); ]
    | SwitchStmt {cond=b; body=s} -> [
        Line ("switch " ^ Expr.to_string b ^ " {");
        Block (stmt_to_s s);
        Line ("}");
      ]
    | CaseStmt c ->
      [ Line ("case " ^ Expr.to_string c.case ^ ":"); Block(stmt_to_s c.body) ]
    | DefaultStmt d ->
      [ Line ("default:"); Block(stmt_to_s d) ]
    | IfStmt {cond=b; then_stmt=s1; else_stmt=s2} ->
      let s1 = stmt_to_s s1 in
      let s2 = stmt_to_s s2 in
      if s1 = [] && s2 = [] then []
      else
        [Line ("if (" ^ Expr.to_string b ^ ")")] @
        ret s1 @
        (if s2 = [] then [] else [ Line "else"; ] @ ret s2)
    | CompoundStmt l ->
      let l = List.concat_map stmt_to_s l in
      if l = [] then [] else ret l
    | DeclStmt [] -> []
    | DeclStmt [d] -> [Line ("decl " ^ Decl.to_string d)]
    | DeclStmt d -> [Line "decl {"; Block (List.map (fun e -> Line (Decl.to_string e)) d); Line "}"]
    | SExpr e -> [Line (Expr.to_string e)]
  in
  stmt_to_s

let summarize_stmt: d_stmt -> string =
  let rec stmt_to_s : d_stmt -> string =
    function
    | WriteAccessStmt w ->
      "rw " ^
      subscript_to_s w.target ^
      " = " ^
      Expr.to_string w.source ^ ";"
    | ReadAccessStmt r -> "ro " ^ Variable.name r.target ^ " = " ^ subscript_to_s r.source ^ ";"
    | ReturnStmt -> "return;"
    | GotoStmt -> "goto;"
    | BreakStmt -> "break;"
    | ContinueStmt -> "continue;"
    | ForStmt f ->
        "for (" ^
        ForInit.opt_to_string f.init ^ "; " ^
        Expr.opt_to_string f.cond ^ "; " ^
        Expr.opt_to_string f.inc ^
        ") {...}"
    | WhileStmt {cond=b; body=s} -> "while (" ^ Expr.to_string b ^ ") {...}"
    | DoStmt {cond=b; body=s} -> "{...} do (" ^ Expr.to_string b ^ ")";
    | SwitchStmt {cond=b; body=s} -> "switch (" ^ Expr.to_string b ^ ") {...}";
    | CaseStmt c -> "case " ^ Expr.to_string c.case ^ ": {...}"
    | DefaultStmt d -> "default: {...}"
    | IfStmt {cond=b; then_stmt=s1; else_stmt=s2} ->
      "if (" ^ Expr.to_string b ^ ") {...} else {...}"
    | CompoundStmt l ->
      let c = List.length l |> string_of_int in
      "{ " ^ c ^ " stmts... }"
    | DeclStmt d ->
      "decl {" ^ Common.join ", " (List.map Decl.to_string d) ^ "}"
    | SExpr e -> Expr.to_string e
  in
  stmt_to_s

let kernel_to_s (k:d_kernel) : PPrint.t list =
  let tps = let open C_lang in if k.type_params <> [] then "[" ^
      list_to_s type_param_to_s k.type_params ^
    "]" else ""
  in
  let open PPrint in
  [
    let open C_lang in
    Line (KernelAttr.to_string k.attribute ^ " " ^ k.name ^ " " ^ tps ^
    "(" ^ list_to_s param_to_s k.params ^ ")");
  ]
  @
  stmt_to_s k.code

let def_to_s (d:d_def) : PPrint.t list =
  let open PPrint in
  match d with
  | Declaration d -> [Line (Decl.to_string d ^ ";")]
  | Kernel k -> kernel_to_s k

let program_to_s (p:d_program) : PPrint.t list =
  List.concat_map (fun k -> def_to_s k @ [Line ""]) p

let print_program (p:d_program) : unit =
  PPrint.print_doc (program_to_s p)

