open Stage0
open Protocols

module StackTrace = Common.StackTrace
module KernelAttr = C_lang.KernelAttr

type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

type array_t = Protocols.Memory.t
type d_type = json

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
    | CXXOperatorCallExpr of {func: t; args: t list; ty: d_type}
    | FloatingLiteral of float
    | IntegerLiteral of int
    | MemberExpr of {name: string; base: t; ty: d_type}
    | Ident of Decl_expr.t
    | UnaryOperator of { opcode: string; child: t; ty: d_type}
    | UnresolvedLookupExpr of {name: Variable.t; tys: d_type list}
  and d_binary = {opcode: string; lhs: t; rhs: t; ty: d_type}

  let ident
    ?(ty=C_type.j_int_type)
    ?(kind=Decl_expr.Kind.Var)
    (name:Variable.t)
  =
    Ident (Decl_expr.from_name ~ty ~kind name)

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
    | CXXOperatorCallExpr _ -> "CXXOperatorCallExpr"
    | FloatingLiteral _ -> "FloatingLiteral"
    | IntegerLiteral _ -> "IntegerLiteral"
    | MemberExpr _ -> "MemberExpr"
    | UnaryOperator _ -> "UnaryOperator"
    | UnresolvedLookupExpr _ -> "UnresolvedLookupExpr"
    | Ident _ -> "Ident"

  let rec to_type :  t -> d_type =
    function
    | SizeOfExpr _ -> C_type.j_int_type
    | CXXNewExpr c -> c.ty
    | CXXDeleteExpr c -> c.ty
    | RecoveryExpr ty -> ty
    | CharacterLiteral _ -> C_type.j_char_type
    | BinaryOperator a -> a.ty
    | ConditionalOperator c -> to_type c.then_expr
    | CXXBoolLiteralExpr _ -> C_type.j_bool_type
    | Ident a -> Decl_expr.ty a
    | CXXConstructExpr c -> c.ty
    | FloatingLiteral _ -> C_type.j_float_type
    | IntegerLiteral _ -> C_type.j_int_type
    | UnaryOperator a -> a.ty
    | CallExpr c -> c.ty
    | CXXOperatorCallExpr a -> a.ty
    | MemberExpr a -> a.ty
    | UnresolvedLookupExpr _ -> C_type.mk_j_type "?"


  let to_string ?(modifier:bool=false) ?(provenance:bool=false) ?(types:bool=false) : t -> string =
    let attr (s:string) : string =
      if modifier
      then "@" ^ s ^ " "
      else ""
    in
    let opcode (o:string) (j:Yojson.Basic.t) : string =
      if types
      then "(" ^ o ^ "." ^ C_type.j_to_string j ^ ")"
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
        | Ident _
        | UnresolvedLookupExpr _
        | CallExpr _
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
      | SizeOfExpr ty -> "sizeof(" ^ C_type.j_to_string ty ^ ")"
      | CXXNewExpr c -> "new " ^ C_type.j_to_string c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
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
      | CXXConstructExpr c -> attr "ctor" ^ C_type.j_to_string c.ty ^ "(" ^ list_to_s exp_to_s c.args ^ ")"
      | CXXOperatorCallExpr c -> exp_to_s c.func ^ "[" ^ list_to_s exp_to_s c.args  ^ "]"
      | Ident v -> Decl_expr.to_string ~modifier v
      | CallExpr c -> par c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
      | UnresolvedLookupExpr v -> attr "unresolv" ^ var_name v.name
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
    | CXXConstructExpr _ -> "ctor"
    | InitListExpr i -> list_to_s Expr.to_string i.args
    | IExpr i -> Expr.to_string i

end

module Decl = struct
  type t = {
    ty_var: Ty_variable.t;
    init: Init.t option;
    attrs: string list
  }

  let make ~ty_var ~init ~attrs : t =
    {ty_var; init; attrs}

  let from_undef ?(attrs=[]) (ty_var:Ty_variable.t) : t =
    {ty_var; init=None; attrs}

  let from_init ?(attrs=[]) (ty_var:Ty_variable.t) (init:Init.t) : t =
    {ty_var; init=Some init; attrs}

  let from_expr ?(attrs=[]) (ty_var:Ty_variable.t) (expr:Expr.t) : t =
    from_init ~attrs ty_var (IExpr expr)

  let get_shared (d:t) : Memory.t option =
    if List.mem C_lang.c_attr_shared d.attrs
    then
      let ty = d.ty_var |> Ty_variable.ty in
      match C_type.from_json ty with
      | Ok ty ->
        Some {
          hierarchy = SharedMemory;
          size = C_type.get_array_length ty;
          data_type = C_type.get_array_type ty;
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
    attr ^ Ty_variable.to_string d.ty_var ^ i

end

module ForInit = struct
  type t =
    | Decls of Decl.t list
    | Expr of Expr.t

  let to_exp (f:t) : Expr.t list =
    match f with
    | Decls l -> List.fold_left
      (fun l d -> Common.append_rev1 (Decl.to_exp d) l)
      []
      l
    | Expr e -> [e]

  (* Returns the binders of a for statement *)
  let loop_vars : t -> Variable.t list =
    let rec exp_var (e:Expr.t) : Variable.t list =
      match e with
      | BinaryOperator {lhs=l; opcode=","; rhs=r; _} ->
        exp_var l |> Common.append_rev1 (exp_var r)
      | BinaryOperator {lhs=Ident x; opcode="="; _} ->
        [Decl_expr.name x]
      | _ -> []
    in
    function
    | Decls l -> List.map (fun (d:Decl.t) -> d.ty_var |> Ty_variable.name) l
    | Expr e -> exp_var e

  let to_string : t -> string =
    function
    | Decls d -> list_to_s Decl.to_string d
    | Expr e -> Expr.to_string e


  let opt_to_string (o:t option) : string =
    o
    |> Option.map to_string
    |> Option.value ~default:""

end

type d_subscript = {name: Variable.t; index: Expr.t list; ty: d_type; location: Location.t}
let subscript_to_s (s:d_subscript) : string =
  Variable.name s.name ^ "[" ^ list_to_s Expr.to_string s.index ^ "]"

type d_write = {
  (* The index *)
  target: d_subscript;
  (* The value being written to the array *)
  source: Expr.t;
  (* A payload is used to detect *benign data-races*. If we are able to identify a
     literal being written to the array, then the value is captured in the
     payload. This particular value is propagated to MAPs.
     *)
  payload: int option
}
type d_read = {target: Variable.t; source: d_subscript}

module Stmt = struct
  type t =
    | WriteAccessStmt of d_write
    | ReadAccessStmt of d_read
    | BreakStmt
    | GotoStmt
    | ReturnStmt
    | ContinueStmt
    | IfStmt of {cond: Expr.t; then_stmt: t; else_stmt: t}
    | CompoundStmt of t list
    | DeclStmt of Decl.t list
    | WhileStmt of d_cond
    | ForStmt of d_for
    | DoStmt of d_cond
    | SwitchStmt of d_cond
    | DefaultStmt of t
    | CaseStmt of {case: Expr.t; body: t}
    | SExpr of Expr.t
  and d_cond = {cond: Expr.t; body: t}
  and d_for = {init: ForInit.t option; cond: Expr.t option; inc: Expr.t option; body: t}

  let last: t -> t * t =
    function
    | CompoundStmt l ->
      (match Common.last l with
      | Some (l, x) -> CompoundStmt l, x
      | None -> CompoundStmt [], CompoundStmt []
      )
    | i -> CompoundStmt [], i

  let to_string: t -> Indent.t list =
    let rec stmt_to_s : t -> Indent.t list =
      let ret l : Indent.t list =
        let open Indent in
        match l with
        | [] -> [Line ";"]
        | [Line "{"; Block l; Line "}"]
        | l -> [Line "{"; Block l; Line "}"]
      in
      let block (s:t) : Indent.t list = ret (stmt_to_s s) in
      function
      | WriteAccessStmt w -> [Line ("rw " ^ subscript_to_s w.target ^ " = " ^ Expr.to_string w.source)]
      | ReadAccessStmt r -> [Line ("ro " ^ Variable.name r.target ^ " = " ^ subscript_to_s r.source)]
      | ReturnStmt -> [Line "return"]
      | GotoStmt -> [Line "goto"]
      | ContinueStmt -> [Line "continue"]
      | BreakStmt -> [Line "break"]
      | ForStmt f ->
        let open Indent in
        [
          Line ("for (" ^ ForInit.opt_to_string f.init ^ "; " ^ Expr.opt_to_string f.cond ^ "; " ^ Expr.opt_to_string f.inc ^ ")");
        ]
        @ block (f.body)
      | WhileStmt {cond=b; body=s} ->
        let open Indent in
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
        let open Indent in
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
      | DeclStmt d ->
        let open Indent in
        [Line "decl {"; Block (List.map (fun e -> Line (Decl.to_string e)) d); Line "}"]
      | SExpr e -> [Line (Expr.to_string e)]
    in
    stmt_to_s

  let summarize: t -> string =
    let stmt_to_s : t -> string =
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
      | WhileStmt {cond=b; _} -> "while (" ^ Expr.to_string b ^ ") {...}"
      | DoStmt {cond=b; _} -> "{...} do (" ^ Expr.to_string b ^ ")";
      | SwitchStmt {cond=b; _} -> "switch (" ^ Expr.to_string b ^ ") {...}";
      | CaseStmt c -> "case " ^ Expr.to_string c.case ^ ": {...}"
      | DefaultStmt _ -> "default: {...}"
      | IfStmt {cond=b; _} ->
        "if (" ^ Expr.to_string b ^ ") {...} else {...}"
      | CompoundStmt l ->
        let c = List.length l |> string_of_int in
        "{ " ^ c ^ " stmts... }"
      | DeclStmt d ->
        "decl {" ^ Common.join ", " (List.map Decl.to_string d) ^ "}"
      | SExpr e -> Expr.to_string e
    in
    stmt_to_s
end

let for_to_expr (f:Stmt.d_for) : Expr.t list =
  let l1 = f.init |> Option.map ForInit.to_exp |> Option.value ~default:[] in
  let l2 = f.cond |> Option.map (fun x -> [x]) |> Option.value ~default:[] in
  let l3 = f.inc |> Option.map (fun x -> [x]) |> Option.value ~default:[] in
  l1
  |> Common.append_rev1 l2
  |> Common.append_rev1 l3

let for_loop_vars (f:Stmt.d_for) : Variable.t list =
  f.init
  |> Option.map ForInit.loop_vars
  |> Option.value ~default:[]

module Kernel = struct
  type t = {
    name: string;
    code: Stmt.t;
    type_params: C_lang.c_type_param list;
    params: C_lang.Param.t list;
    attribute: KernelAttr.t;
  }
  let is_global (k:t) : bool =
    k.attribute |> KernelAttr.is_global
end

module Def = struct
  type t =
    | Kernel of Kernel.t
    | Declaration of Decl.t

  let is_device_kernel : t -> bool =
    function
    | Kernel k when Kernel.is_global k -> true
    | _ -> false
end

type d_program = Def.t list

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
  type t = Stmt.t list

  let counter = ref 1

  let make_empty = []

  let add_var (lbl:string) (f:Variable.t -> Stmt.t list) (st:t) : (t * Variable.t) =
    let count = !counter in
    counter := count + 1;
    let name : string = "_unknown_" ^ string_of_int count in
    let x = {Variable.name=name; Variable.label=Some lbl;Variable.location=None} in
    (f x @ st, x)

  let add_stmt (s: Stmt.t) (st:t) : t = s :: st

  let add_expr (expr:Expr.t) (ty:d_type) (st:t) : t * Variable.t =
    add_var (Expr.to_string expr) (fun name ->
      [
        let ty_var = Ty_variable.make ~ty ~name in
        DeclStmt [Decl.from_expr ty_var expr]
      ]
    ) st


  let add_write (a:d_subscript) (source:Expr.t) (payload:int option) (st:t) : (t * Variable.t) =
    let wr x = Stmt.WriteAccessStmt {
      target=a;
      source=Ident {x with ty=a.ty};
      payload
    } in
    match source with
    | Ident x ->
      (add_stmt (wr x) st, Decl_expr.name x)
    | _ ->
      add_var (subscript_to_s a) (fun x ->
        [
          wr (Decl_expr.from_name x);
          let ty_var = Ty_variable.make ~name:x ~ty:a.ty in
          DeclStmt [Decl.from_expr ty_var source];
        ]
      ) st

  let add_read (a:d_subscript) (st:t) : (t * Variable.t) =
    add_var (subscript_to_s a) (fun x ->
      [
        ReadAccessStmt {target=x; source=a};
      ]
    ) st
end

let rec rewrite_exp (c:C_lang.Expr.t) : (AccessState.t, Expr.t) state =
  let open Expr in
  match c with
  | CXXOperatorCallExpr {
      func=Ident {name=v; _};
      args=[ArraySubscriptExpr a; src];
      _
    } when Variable.name v = "operator="
    -> rewrite_write a src

  | SizeOfExpr ty ->
    fun st -> (st, SizeOfExpr ty)

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

  | Ident v -> state_pure (Ident v)
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

    | Ident {name; ty; _} ->
      state_pure {name; index=indices; ty; location=Option.get loc} st

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
    let payload = match src with
      | IntegerLiteral x -> Some x
      | _ -> None
    in
    let (st, x) = AccessState.add_write a src' payload st in
  state_pure (Expr.ident ~ty:(C_lang.Expr.to_type src) x) st
and rewrite_read (a:C_lang.Expr.c_array_subscript): (AccessState.t, Expr.t) state =
  fun st ->
    let (st, a) = rewrite_subscript a st in
    let (st, x) = AccessState.add_read a st in
    state_pure (Expr.ident ~ty:a.ty x) st

let map_opt (f:'a -> ('s * 'b)) (o:'a option) : ('s * 'b option) =
  match o with
  | Some v ->
    let (st, v) = f v in
    (st, Some v)
  | None -> ([], None)


let rewrite_exp (e:C_lang.Expr.t) : (Stmt.t list * Expr.t) =
  let (st, e) = rewrite_exp e AccessState.make_empty in
  (st |> List.rev, e)

let rewrite_exp_list (es:C_lang.Expr.t list) : (Stmt.t list * Expr.t list) =
  let (ss, es) = List.map rewrite_exp es |> List.split in
  (List.concat ss, es)

let rewrite_decl (d:C_lang.Decl.t) : (Stmt.t list * Decl.t) =
  let rewrite_init (c:C_lang.Init.t) : (Stmt.t list * Init.t) =
    match c with
    | InitListExpr {ty=ty; args=args} ->
      let (pre, args) = rewrite_exp_list args in
      (pre, InitListExpr {ty=ty; args=args})
    | IExpr e ->
      let (pre, e) = rewrite_exp e in
      (pre, IExpr e)
  in
  let (pre, o) = d |> C_lang.Decl.init |> map_opt rewrite_init in
  (pre, {ty_var=C_lang.Decl.ty_var d; init=o; attrs=C_lang.Decl.attrs d})

let rewrite_for_init (f:C_lang.ForInit.t) : (Stmt.t list * ForInit.t) =
  match f with
  | Decls d ->
    let (pre, d) = List.map rewrite_decl d |> List.split in
    (List.concat pre, Decls d)
  | Expr e ->
    let (s, e) = rewrite_exp e in
    (s, Expr e)

let rec rewrite_stmt (s:C_lang.Stmt.t) : Stmt.t list =
  let decl (pre:Stmt.t list) (s:Stmt.t) =
    match pre with
    | [] -> [s]
    | _ -> [CompoundStmt (pre @ [s])]
  in
  let rewrite_s (s:C_lang.Stmt.t) : Stmt.t =
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

let rewrite_kernel (k:C_lang.Kernel.t) : Kernel.t =
  let rewrite_s (s:C_lang.Stmt.t) : Stmt.t =
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

let rewrite_def (d:C_lang.c_def) : Def.t =
  match d with
  | Kernel k -> Kernel (rewrite_kernel k)
  | Declaration d ->
    let (_, d) = rewrite_decl d in
    Declaration d

let rewrite_program: C_lang.c_program -> d_program =
  List.map rewrite_def

(* ------------------------------------------------------------------------ *)

let kernel_to_s (k:Kernel.t) : Indent.t list =
  let tps = let open C_lang in if k.type_params <> [] then "[" ^
      list_to_s type_param_to_s k.type_params ^
    "]" else ""
  in
  let open Indent in
  [
    let open C_lang in
    Line (KernelAttr.to_string k.attribute ^ " " ^ k.name ^ " " ^ tps ^
    "(" ^ list_to_s Param.to_string k.params ^ ")");
  ]
  @
  Stmt.to_string k.code

let def_to_s (d:Def.t) : Indent.t list =
  let open Indent in
  match d with
  | Declaration d -> [Line (Decl.to_string d ^ ";")]
  | Kernel k -> kernel_to_s k

let program_to_s (p:d_program) : Indent.t list =
  List.concat_map (fun k -> def_to_s k @ [Line ""]) p

let print_program (p:d_program) : unit =
  Indent.print (program_to_s p)
