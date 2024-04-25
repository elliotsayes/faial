open Stage0
open Protocols

module StackTrace = Common.StackTrace

open Exp
type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

let list_to_s (f:'a -> string) (l:'a list) : string =
  List.map f l |> Common.join ", "

module Expr = struct
  type t =
    | SizeOfExpr of J_type.t
    | CXXNewExpr of {arg: t; ty: J_type.t}
    | CXXDeleteExpr of {arg: t; ty: J_type.t}
    | RecoveryExpr of J_type.t
    | CharacterLiteral of int
    | ArraySubscriptExpr of c_array_subscript
    | BinaryOperator of c_binary
    | CallExpr of {func: t; args: t list; ty: J_type.t}
    | ConditionalOperator of {cond: t; then_expr: t; else_expr: t; ty: J_type.t}
    | CXXConstructExpr of {args: t list; ty: J_type.t}
    | CXXBoolLiteralExpr of bool
    | Ident of Decl_expr.t
    | CXXOperatorCallExpr of {func: t; args: t list; ty: J_type.t}
    | FloatingLiteral of float
    | IntegerLiteral of int
    | MemberExpr of {name: string; base: t; ty: J_type.t}
    | UnaryOperator of {opcode: string; child: t; ty: J_type.t}
    | UnresolvedLookupExpr of {name: Variable.t; tys: J_type.t list}
  and c_binary = {opcode: string; lhs: t; rhs: t; ty: J_type.t}
  and c_array_subscript = {lhs: t; rhs: t; ty: J_type.t; location: Location.t}

  let rec to_type : t -> J_type.t =
    function
    | SizeOfExpr _ -> J_type.int
    | CXXNewExpr c -> c.ty
    | CXXDeleteExpr c -> c.ty
    | CXXConstructExpr c -> c.ty
    | CharacterLiteral _ -> J_type.char
    | ArraySubscriptExpr a -> a.ty
    | BinaryOperator a -> a.ty
    | ConditionalOperator c -> to_type c.then_expr
    | CXXBoolLiteralExpr _ -> J_type.bool
    | FloatingLiteral _ -> J_type.float
    | Ident a -> a.ty
    | IntegerLiteral _ -> J_type.int
    | UnaryOperator a -> a.ty
    | CallExpr c -> c.ty
    | CXXOperatorCallExpr a -> a.ty
    | MemberExpr a -> a.ty
    | UnresolvedLookupExpr _ -> J_type.unknown
    | RecoveryExpr ty -> ty

  let to_string ?(modifier:bool=false) ?(provenance:bool=false) ?(types:bool=false) : t -> string =
    let attr (s:string) : string =
      if modifier
      then "@" ^ s ^ " "
      else ""
    in
    let opcode (o:string) (j:J_type.t) : string =
      if types
      then "(" ^ o ^ "." ^ J_type.to_string j ^ ")"
      else o
    in
    let var_name : Variable.t -> string =
      if provenance
      then Variable.name_line
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
      | SizeOfExpr ty -> "sizeof(" ^ J_type.to_string ty ^ ")"
      | CXXNewExpr c -> "new " ^ J_type.to_string c.ty ^ "(" ^ exp_to_s c.arg ^ ")"
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
      | CXXConstructExpr c -> attr "ctor" ^ J_type.to_string c.ty ^ "(" ^ list_to_s exp_to_s c.args ^ ")"
      | CXXOperatorCallExpr c -> exp_to_s c.func ^ "[" ^ list_to_s exp_to_s c.args  ^ "]"
      | Ident v -> Decl_expr.to_string ~modifier v
      | CallExpr c -> par c.func ^ "(" ^ list_to_s exp_to_s c.args  ^ ")"
      | UnresolvedLookupExpr v -> attr "unresolv" ^ var_name v.name
      | UnaryOperator u -> u.opcode ^ par u.child
    in
    exp_to_s

  let opt_to_string : t option -> string =
    function
    | Some o -> to_string o
    | None -> ""


  module Visit = struct
    type expr_t = t

    type 'a t =
      | SizeOf of J_type.t
      | CXXNew of {arg: 'a; ty: J_type.t}
      | CXXDelete of {arg: 'a; ty: J_type.t}
      | Recovery of J_type.t
      | CharacterLiteral of int
      | ArraySubscript of {lhs: 'a; rhs: 'a; ty: J_type.t; location: Location.t}
      | BinaryOperator of {opcode: string; lhs: 'a; rhs: 'a; ty: J_type.t}
      | Call of {func: 'a; args: 'a list; ty: J_type.t}
      | ConditionalOperator of {cond: 'a; then_expr: 'a; else_expr: 'a; ty: J_type.t}
      | CXXConstruct of {args: 'a list; ty: J_type.t}
      | CXXBoolLiteral of bool
      | Ident of Decl_expr.t
      | CXXOperatorCall of {func: 'a; args: 'a list; ty: J_type.t}
      | FloatingLiteral of float
      | IntegerLiteral of int
      | Member of {name: string; base: 'a; ty: J_type.t}
      | UnaryOperator of {opcode: string; child: 'a; ty: J_type.t}
      | UnresolvedLookup of {name: Variable.t; tys: J_type.t list}

    let rec fold (f: 'a t -> 'a) : expr_t -> 'a =
      function
      | SizeOfExpr e -> f (SizeOf e)
      | CXXNewExpr e -> f (CXXNew {arg=fold f e.arg; ty=e.ty})
      | CXXDeleteExpr e -> f (CXXDelete {arg=fold f e.arg; ty=e.ty})
      | RecoveryExpr e -> f (Recovery e)
      | CharacterLiteral e -> f (CharacterLiteral e)
      | ArraySubscriptExpr e -> f (ArraySubscript {lhs=fold f e.lhs; rhs=fold f e.rhs; ty=e.ty; location=e.location})
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
      | Ident e -> f (Ident e)
      | CXXOperatorCallExpr e -> f (CXXOperatorCall {
          func=fold f e.func;
          args=List.map (fold f) e.args;
          ty=e.ty
        })
      | FloatingLiteral e -> f (FloatingLiteral e)
      | IntegerLiteral e -> f (IntegerLiteral e)
      | MemberExpr e -> f (Member {
          name=e.name;
          base=fold f e.base;
          ty=e.ty
        })
      | UnaryOperator e -> f (UnaryOperator {
          opcode=e.opcode;
          child=fold f e.child;
          ty=e.ty
        })
      | UnresolvedLookupExpr e ->
        f (UnresolvedLookup {name=e.name; tys=e.tys})

    let rec map (f: expr_t -> expr_t) (e: expr_t) : expr_t =
      let ret : expr_t -> expr_t = map f in
      match e with
        | FloatingLiteral _
        | IntegerLiteral _
        | CharacterLiteral _
        | Ident _
        | RecoveryExpr _
        | SizeOfExpr _
        | UnresolvedLookupExpr _
        | CXXBoolLiteralExpr _
          -> f e
        | CXXNewExpr {arg=a; ty=ty} ->
          f (CXXNewExpr {arg=ret a; ty=ty})
        | CXXDeleteExpr {arg=a; ty=ty} ->
          f (CXXDeleteExpr {arg=ret a; ty=ty})
        | ArraySubscriptExpr {lhs=e1; rhs=e2; ty=ty; location=l} ->
          f (ArraySubscriptExpr {lhs=ret e1; rhs=ret e2; ty=ty; location=l})
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

end

module Init = struct
  type t =
    | InitListExpr of {ty: J_type.t; args: Expr.t list}
    | IExpr of Expr.t

  let map_expr (f:Expr.t -> Expr.t) : t -> t =
    function
    | InitListExpr {ty=ty; args=l} ->
      InitListExpr {ty=ty; args=List.map f l}
    | IExpr e -> IExpr (f e)

  let to_expr_seq : t -> Expr.t Seq.t =
    function
    | InitListExpr l -> List.to_seq l.args
    | IExpr e -> Seq.return e

  let to_string : t -> string =
    function
    | InitListExpr i -> list_to_s Expr.to_string i.args
    | IExpr i -> Expr.to_string i
end

let c_attr (k:string) : string =
  " __attribute__((" ^ k ^ "))"

let c_attr_shared = c_attr "shared"
let c_attr_global = c_attr "global"
let c_attr_device = c_attr "device"

module Decl : sig
  type t = {
    var: Variable.t;
    ty: J_type.t;
    init: Init.t option;
    attrs: string list
  }

  (* Constructor *)
  val make :
    ty_var:Ty_variable.t ->
    init:Init.t option ->
    attrs:string list ->
    t
  (* Accessors *)
  val attrs: t -> string list
  val init : t -> Init.t option
  (* Expression iterator *)
  val to_expr_seq : t -> Expr.t Seq.t
  (* Update contained expressions *)
  val map_expr : (Expr.t -> Expr.t) -> t -> t
  (* Show its contents *)
  val to_string : t -> string
  (* Convinience *)
  val is_shared : t -> bool
  val matches : (C_type.t -> bool) -> t -> bool
  val var: t -> Variable.t
  val ty: t -> J_type.t
  val to_s :  t -> Indent.t list

end = struct
  type t = {
    var: Variable.t;
    ty: J_type.t;
    init: Init.t option;
    attrs: string list
  }

  let make ~ty_var ~init ~attrs : t =
    {ty=ty_var.ty; var=Ty_variable.name ty_var; init; attrs}

  let init (x:t) : Init.t option = x.init

  let attrs (x:t) : string list = x.attrs

  let var (x:t) : Variable.t = x.var

  let ty (x:t) : J_type.t = x.ty

  let matches pred x = J_type.matches pred x.ty

  let is_shared (x:t) : bool =
    List.mem c_attr_shared x.attrs

  let to_expr_seq (x:t) : Expr.t Seq.t =
    match x.init with
    | Some i -> Init.to_expr_seq i
    | None -> Seq.empty

  let map_expr (f: Expr.t -> Expr.t) (x:t) : t =
    { x with init=x.init |> Option.map (Init.map_expr f) }

  let to_string (d: t): string =
    let i = match d.init with
      | Some e -> " = " ^ Init.to_string e
      | None -> ""
    in
    let attr = if d.attrs = [] then "" else
      let attrs = Common.join " " d.attrs |> String.trim in
      attrs ^ " "
    in
    attr ^ J_type.to_string d.ty ^ " " ^ Variable.name d.var ^ i

  let to_s (d:t) : Indent.t list =
    [Line (to_string d ^ ";")]
end

module ForInit = struct
  type t =
    | Decls of Decl.t list
    | Expr of Expr.t

  (* Iterate over the expressions contained in a for-init *)
  let to_expr_seq : t -> Expr.t Seq.t =
    function
    | Decls l -> List.to_seq l |> Seq.concat_map Decl.to_expr_seq
    | Expr e -> Seq.return e

  (* Returns the binders of a for statement *)
  let loop_vars : t -> Variable.t list =
    let rec exp_var (e:Expr.t) : Variable.t list =
      match e with
      | BinaryOperator {lhs=l; opcode=","; rhs=r; _} ->
        exp_var l |> Common.append_rev1 (exp_var r)
      | BinaryOperator {lhs=Ident l; opcode="="; _} ->
        [l.name]
      | _ -> []
    in
    function
    | Decls l -> List.map Decl.var l
    | Expr e -> exp_var e

  let to_string : t -> string =
    function
    | Decls d -> list_to_s Decl.to_string d
    | Expr e -> Expr.to_string e

  let opt_to_string : t option -> string =
    function
    | Some o -> to_string o
    | None -> ""

end

module Stmt = struct
  type 'a if_t = {cond: Expr.t; then_stmt: 'a; else_stmt: 'a}
  type 'a cond_t = {cond: Expr.t; body: 'a}
  type 'a for_t = {init: ForInit.t option; cond: Expr.t option; inc: Expr.t option; body: 'a}
  type 'a case_t = {case: Expr.t; body: 'a}
  type t =
    | BreakStmt
    | GotoStmt
    | ReturnStmt of Expr.t option
    | ContinueStmt
    | IfStmt of t if_t
    | CompoundStmt of t list
    | DeclStmt of Decl.t list
    | WhileStmt of t cond_t
    | ForStmt of t for_t
    | DoStmt of t cond_t
    | SwitchStmt of t cond_t
    | DefaultStmt of t
    | CaseStmt of t case_t
    | SExpr of Expr.t

  type if_stmt = t if_t
  type cond_stmt = t cond_t
  type for_stmt = t for_t
  type case_stmt = t for_t

  let to_string : t -> Indent.t list
  =
    let ret l : Indent.t list =
      let open Indent in
      match l with
      | [] -> [Line ";"]
      | [Line "{"; Block l; Line "}"]
      | l -> [Line "{"; Block l; Line "}"]
    in
    let rec stmt_to_s : t -> Indent.t list =
      let block (s:t) : Indent.t list = ret (stmt_to_s s) in
      function
      | ReturnStmt None -> [Line "return"]
      | ReturnStmt (Some e) -> [Line ("return" ^ Expr.to_string e)]
      | GotoStmt -> [Line "goto"]
      | BreakStmt -> [Line "break"]
      | ContinueStmt -> [Line "continue"]
      | ForStmt f ->
        let open Indent in
        [
          Line ("for (" ^ ForInit.opt_to_string f.init ^ "; " ^ Expr.opt_to_string f.cond ^ "; " ^ Expr.opt_to_string f.inc ^ ")");
        ]
        @ block (f.body)
      | WhileStmt {cond=b; body=s} -> [
          Line ("while (" ^ Expr.to_string b ^ ") {");
          Block (stmt_to_s s);
          Line "}"
        ]
      | DoStmt {cond=b; body=s} -> [
          Line "}";
          Block (stmt_to_s s);
          Line ("do (" ^ Expr.to_string b ^ ") {");
        ]
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
      | CompoundStmt [] -> []
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

  module Visit = struct
    type c_stmt = t

    type 'a t =
      | Break
      | Goto
      | Return of Expr.t option
      | Continue
      | If of 'a if_t
      | Compound of 'a list
      | Decl of Decl.t list
      | While of 'a cond_t
      | For of 'a for_t
      | Do of 'a cond_t
      | Switch of 'a cond_t
      | Default of 'a
      | Case of 'a case_t
      | SExpr of Expr.t

    let rec fold (f: 'a t -> 'a) : c_stmt -> 'a =
      function
      | BreakStmt -> f Break
      | GotoStmt -> f Goto
      | ReturnStmt e -> f (Return e)
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
      | SExpr e -> f (SExpr e)

    let map (f: c_stmt -> c_stmt) : c_stmt -> c_stmt =
      fold (
        function
        | Break -> f BreakStmt
        | Goto -> f GotoStmt
        | Return e -> f (ReturnStmt e)
        | Continue -> f ContinueStmt
        | If c -> f (IfStmt c)
        | Compound l -> f (CompoundStmt l)
        | Decl l -> f (DeclStmt l)
        | While c -> f (WhileStmt c)
        | For c -> f (ForStmt c)
        | Do c -> f (DoStmt c)
        | Switch c -> f (SwitchStmt c)
        | Default c -> f (DefaultStmt c)
        | Case c -> f (CaseStmt c)
        | SExpr e -> f (SExpr e)
      )

    let to_expr_seq: c_stmt -> Expr.t Seq.t =
      fold (function
        | Break | Goto | Return None | Continue -> Seq.empty
        | Return (Some e) -> Seq.return e
        | If {cond=c; then_stmt=s1; else_stmt=s2} ->
          Seq.return c
          |> Seq.append s1
          |> Seq.append s2
        | Compound l -> List.to_seq l |> Seq.concat
        | Decl d ->
          List.to_seq d
          |> Seq.concat_map (fun d ->
            Decl.init d
            |> Option.to_seq
            |> Seq.concat_map Init.to_expr_seq
          )
        | While {cond=c; body=b}
        | Do {cond=c; body=b}
        | Switch {cond=c; body=b}
        | Case {case=c; body=b}
          -> Seq.cons c b
        | For s ->
          Option.to_seq s.init
          |> Seq.concat_map ForInit.to_expr_seq
        | Default s -> s
        | SExpr e -> Seq.return e
      )

  end

  let rec find (f:t -> bool) (s:t) : t option =
    if f s then Some s
    else
    match s with
    | BreakStmt
    | GotoStmt
    | ReturnStmt _
    | ContinueStmt
    | DeclStmt _
    | SExpr _ ->
      None
    | IfStmt {then_stmt=s1; else_stmt=s2; _} ->
      (match find f s1 with
      | Some s -> Some s
      | None -> find f s2)
    | CompoundStmt l ->
      let rec find_l : t list -> t option =
        function
        | [] -> None
        | s :: l ->
          if f s
          then Some s
          else find_l l
      in
      find_l l
    | WhileStmt {body= s; _}
    | DoStmt {body = s; _}
    | SwitchStmt {body= s; _}
    | CaseStmt {body= s; _}
    | DefaultStmt s
    | ForStmt {body= s; _} ->
      find f s

  let member (f:t -> bool) (s:t) : bool =
    find f s |> Option.is_some

  let rec fold : 'a. (t -> 'a -> 'a) -> t -> 'a -> 'a =
    fun f (s:t) (init:'a) ->
    let init : 'a = f s init in
    match s with
    | BreakStmt
    | GotoStmt
    | ReturnStmt _
    | ContinueStmt
    | DeclStmt _
    | SExpr _ ->
      init
    | IfStmt {then_stmt=s1; else_stmt=s2; _} ->
      let init : 'a = fold f s1 init in
      fold f s2 init
    | CompoundStmt l ->
      List.fold_right (fold f) l init
    | WhileStmt {body= s; _}
    | DoStmt {body = s; _}
    | SwitchStmt {body= s; _}
    | CaseStmt {body= s; _}
    | DefaultStmt s
    | ForStmt {body= s; _} ->
      fold f s init

  (* Returns all elements that match a given predicate *)
  let find_all_map (f: t -> 'a option) (s: t) : 'a Seq.t =
    let g (e:t) (r:'a Seq.t) : 'a Seq.t =
      match f e with
      | Some x -> Seq.cons x r
      | None -> r
    in
    fold g s Seq.empty


  let find_all (f: t -> bool) : t -> t Seq.t =
    find_all_map (fun x -> if f x then Some x else None)

end

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

module Kernel = struct
  type t = {
    name: string;
    ty: string;
    code: Stmt.t;
    type_params: Ty_param.t list;
    params: Param.t list;
    attribute: KernelAttr.t;
  }
  let make ~ty ~name ~code ~type_params ~params ~attribute =
    {name; ty; code; type_params; params; attribute}
  let name (x:t) : string = x.name
  let params (x:t) : Param.t list = x.params
  let type_params (x:t) : Ty_param.t list = x.type_params
  let attribute (x:t) : KernelAttr.t = x.attribute

  let to_s (k:t) : Indent.t list =
    let tps = if k.type_params <> [] then "[" ^
        list_to_s Ty_param.to_string k.type_params ^
      "]" else ""
    in
    let open Indent in
    [
      Line (KernelAttr.to_string k.attribute ^ " " ^ k.name ^
        " " ^ tps ^ "(" ^ list_to_s Param.to_string k.params ^ ")");
    ]
    @
    Stmt.to_string k.code

end

module Def = struct
  type t =
    | Kernel of Kernel.t
    | Declaration of Decl.t
    | Typedef of Typedef.t
    | Enum of Enum.t


  let to_s (d:t) : Indent.t list =
    match d with
    | Declaration d -> Decl.to_s d
    | Kernel k -> Kernel.to_s k
    | Typedef d -> Typedef.to_s d
    | Enum e -> Enum.to_s e
end

module Program = struct
  type t = Def.t list

  let rewrite_shared_arrays: t -> t =
    (* Rewrites expressions: when it finds a variable that has been defined as
      a shared variable, we replace that by an array subscript:
      x becomes x[0] *)
    let rw_exp (vars:Variable.Set.t) (e:Expr.t) : Expr.t =
      if Variable.Set.is_empty vars then e else
      e |> Expr.Visit.map (fun e ->
        match e with
        | Ident x ->
          if Variable.Set.mem x.name vars
          then ArraySubscriptExpr {
            lhs=Ident x;
            rhs=IntegerLiteral 0;
            ty=x.ty;
            location=Variable.location x.name
          }
          else e
        | _ -> e)
    in
    (* When rewriting a variable declaration, we must return as the side-effect
      the shadowing of the available variables when it makes sense *)
    let rw_decl (vars:Variable.Set.t) (d:Decl.t) : Variable.Set.t * Decl.t =
      let vars =
        let name = d |> Decl.var in
        if Decl.is_shared d && not (Decl.matches C_type.is_array d) then
          Variable.Set.add name vars
        else
          Variable.Set.remove name vars
      in
      (vars, Decl.map_expr (rw_exp vars) d)
    in
    (* This is just a monadic map *)
    let rec rw_list: 'a. (Variable.Set.t -> 'a -> Variable.Set.t * 'a) -> Variable.Set.t -> 'a list -> Variable.Set.t * 'a list =
      fun f vars l ->
          match l with
          | [] -> (vars, [])
          | x :: l ->
            let (vars, x) = f vars x in
            let (vars, l) = rw_list f vars l in
            (vars, x::l)
    in
    (* We now rewrite statements *)
    let rw_stmt (vars:Variable.Set.t) (s:Stmt.t) : Stmt.t =
      let rec rw_s (vars:Variable.Set.t) (s: Stmt.t) : Variable.Set.t * Stmt.t =
        let ret (s: Stmt.t) : Stmt.t = rw_s vars s |> snd in
        let rw_e: Expr.t -> Expr.t = rw_exp vars in
        match s with
        | BreakStmt
        | GotoStmt
        | ReturnStmt None
        | ContinueStmt ->
          (vars, s)
        | ReturnStmt (Some e) ->
          (vars, ReturnStmt (Some (rw_e e)))
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
          | Some (ForInit.Decls l) ->
            let (vars, l) = rw_list rw_decl vars l in
            (vars, Some (ForInit.Decls l))
          | Some (ForInit.Expr e) -> (vars, Some (ForInit.Expr (rw_e e)))
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
        | SExpr e ->
          (vars, SExpr (rw_e e))
      in
      rw_s vars s |> snd
    in
    let rec rw_p (vars:Variable.Set.t): t -> t =
      function
      | Declaration d :: p ->
        let vars = if Decl.is_shared d && not (Decl.matches C_type.is_array d)
          then Variable.Set.add (Decl.var d) vars
          else vars
        in
        Declaration d :: rw_p vars p
      | Kernel k :: p -> Kernel { k with code = rw_stmt vars k.code } :: rw_p vars p
      | Typedef d :: p ->
        Typedef d :: rw_p vars p
      | Enum e :: p ->
        Enum e :: rw_p vars p
      | [] -> []
    in
    rw_p Variable.Set.empty

  let to_s (p:t) : Indent.t list =
    List.concat_map (fun k -> Def.to_s k @ [Line ""]) p

  let print (p:t) : unit =
    Indent.print (to_s p)

end

(* ------------------------------------------------------------------- *)

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind


let rec parse_position ?(filename="") : json -> Location.t j_result =
  let open Rjson in
  fun (j:json) ->
    let* o = cast_object j in
    match (
      let* line = with_field "line" cast_int o in
      let line = Index.from_base1 line in
      let* col = with_field "col" cast_int o in
      let interval = Index.from_base1 col |> Interval.from_start in
      let* filename:string = with_field_or "file" cast_string filename o in
      Ok (Location.make
          ~filename
          ~line
          ~interval)
    ) with
    | Ok p -> Ok p
    | Error _ -> with_field "expansionLoc" parse_position o
    

let parse_location (j:json) : Location.t j_result =
  let open Rjson in
  let open Location in
  let* o = cast_object j in
  let* first = with_field "begin" parse_position o in
  let last =
    o
    |> with_field "end" (parse_position ~filename:first.filename)
    |> Result.value ~default:first
  in
  Ok (Location.add_or_reset_lhs first last)

let parse_variable (j:json) : Variable.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* name = with_field "name" cast_string o in
  match List.assoc_opt "range" o with
  | Some range ->
    let* l = parse_location range in
    let l =
      if Location.length l = 0
      then Location.set_length (String.length name) l
      else l
    in
    Ok (Variable.make ~location:l ~name)
  | None -> Ok (Variable.from_name name)
  

let compound (ty:J_type.t) (lhs:Expr.t) (opcode:string) (rhs:Expr.t) : Expr.t =
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
  |> Result.value ~default:None
  |> Option.value ~default:false


let expect_kind (k:string) (o:j_object) : unit j_result =
  let open Rjson in
  let* obtained : string = get_kind o in
  if obtained = k then
    Ok ()
  else
    root_cause ("Expecting kind '"^ k ^"' but got '" ^ obtained ^ "'") (`Assoc o)

let rec parse_exp (j:json) : Expr.t j_result =
  let open Rjson in
  let open Expr in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | _ when is_invalid o ->
    (* Unknown value *)
    let* ty = get_field "type" o in
    Ok (RecoveryExpr (J_type.from_json ty))

  | "CXXDefaultArgExpr"
  | "ImplicitValueInitExpr"
  | "CXXNullPtrLiteralExpr"
  | "StringLiteral"
  | "DependentScopeDeclRefExpr"
  | "RecoveryExpr" ->
    (* Unknown value *)
    let* ty = get_field "type" o in
    Ok (RecoveryExpr (J_type.from_json ty))

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
    Ok (MemberExpr {name=n; base=b; ty=J_type.from_json ty})

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
    let i =
      try
        int_of_string i
      with
        Failure _ ->
          prerr_endline ("Could not parse integer: " ^ i);
          Int.max_int
    in
    Ok (IntegerLiteral i)

  | "MemberExpr" ->
    let* n = with_field "name" cast_string o in
    let* b = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    Ok (MemberExpr {name=n; base=b; ty=J_type.from_json ty})

  | "EnumConstantDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Ident {name; ty=J_type.from_json ty; kind=EnumConstant})

  | "VarDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Ident {name; ty=J_type.from_json ty; kind=Var})
    
  | "FunctionDecl" ->
    let* v = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Ident {name=v; ty=J_type.from_json ty; kind=Function})

  | "CXXMethodDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Ident {name; ty=J_type.from_json ty; kind=CXXMethod})

  | "ConditionalOperator" ->
    let* (c, t, e) = with_field "inner"
      (cast_list_3 parse_exp parse_exp parse_exp) o
    in
    let* ty = get_field "type" o in
    Ok (ConditionalOperator
      {cond=c; then_expr=t; else_expr=e; ty=J_type.from_json ty})

  | "UnaryExprOrTypeTraitExpr" ->
    let* ty = get_field "type" o in
    Ok (SizeOfExpr (J_type.from_json  ty))

  | "ParmVarDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Ident {name; ty=J_type.from_json ty; kind=ParmVar})

  | "NonTypeTemplateParmDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Ident {name; ty=J_type.from_json ty; kind=NonTypeTemplateParm})

  | "UnresolvedLookupExpr" ->
    let* v = parse_variable j in
    let* tys = get_field "lookups" o >>= cast_list in
    Ok (UnresolvedLookupExpr {name=v; tys=List.map J_type.from_json tys})

  | "CXXNewExpr" ->
    let* arg = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    Ok (CXXNewExpr {arg=arg; ty=J_type.from_json ty})

  | "CXXDeleteExpr" ->
    let* arg = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    Ok (CXXDeleteExpr {arg=arg; ty=J_type.from_json ty})

  | "UnaryOperator" ->
    let* op = with_field "opcode" cast_string o in
    let* c = with_field "inner" (cast_list_1 parse_exp) o in
    let* ty = get_field "type" o in
    let inc o =
      BinaryOperator {ty=J_type.from_json ty; opcode="="; lhs=c;
        rhs=BinaryOperator{ty=J_type.from_json ty; opcode=o; lhs=c; rhs=IntegerLiteral 1}}
    in
    Ok (match op with
    | "++" -> inc "+"
    | "--" -> inc "-"
    | "+" -> c
    | "-" -> BinaryOperator {ty=J_type.from_json ty; opcode=op; lhs=IntegerLiteral 0; rhs=c}
    | _ -> UnaryOperator {ty=J_type.from_json ty; opcode=op; child=c})

  | "CompoundAssignOperator" ->
    (* Convert: x += e into x = x + y *)
    let* ty = get_field "computeResultType" o in
    let* lhs, rhs = with_field "inner" (cast_list_2 parse_exp parse_exp) o in
    let* opcode = with_field "opcode" cast_string o in
    (match Common.rsplit '=' opcode with
      | Some (opcode, "") -> Ok (compound (J_type.from_json ty) lhs opcode rhs)
      | _ -> root_cause "ERROR: parse_exp" j)

  | "BinaryOperator" ->
    let ty = List.assoc_opt "type" o
      |> Option.map J_type.from_json
      |> Option.value ~default:J_type.int
    in
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
    let* loc = with_field "range" parse_location o in
    Ok (ArraySubscriptExpr {
      ty=J_type.from_json ty;
      lhs=lhs;
      rhs=rhs;
      location=Location.set_length (Location.length loc + 1) loc
    })

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
      | Ident {name=n; kind=CXXMethod; _}, [lhs; rhs] when Variable.name n = "operator=" ->
        BinaryOperator {lhs=lhs; opcode="="; rhs=rhs; ty=to_type lhs}
      | (UnresolvedLookupExpr {name=n; _}, [lhs; rhs])
      | (Ident {name=n; kind=Function; _}, [lhs; rhs]) ->
        let ty = J_type.from_json ty in
        (match Variable.name n with
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
      | _ -> CXXOperatorCallExpr {func=func; args=args; ty=J_type.from_json ty}
    )

  | "CallExpr" ->
    let* (func, args) = with_field "inner" (fun j ->
      let* h, t = cast_cons j in
      let* func = wrap parse_exp (fun _ -> "func", j) h in
      let* args = wrap (map parse_exp) (fun _ -> "args", j) t in
      Ok (func, args)
    ) o in
    let* ty = get_field "type" o in
    Ok (CallExpr {func=func; args=args; ty=J_type.from_json ty})

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
    Ok (CXXConstructExpr {args=args; ty=J_type.from_json ty})


  | "CXXBoolLiteralExpr" ->
    let* b = with_field "value" cast_bool o in
    Ok (CXXBoolLiteralExpr b)

  | _ ->
    root_cause "ERROR: parse_exp" j

let parse_init (j:json) : Init.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "ParenListExpr"
  | "InitListExpr" ->
    let* ty = get_field "type" o in
    let* args = with_field_or "inner" (cast_map parse_exp) [] o in
    let open Init in
    Ok (InitListExpr {ty=J_type.from_json ty; args=args})

  | _ ->
    let* e = parse_exp j in
    let open Init in
    Ok (IExpr e)

let parse_attr (j:Yojson.Basic.t) : string j_result =
  let open Rjson in
  let* o = cast_object j in
  with_field "value" cast_string o

let is_valid_j : json -> bool =
  function
  | `Assoc o ->
    (match Rjson.get_kind o with
      | Error _ | Ok "FullComment" -> false
      | Ok _ -> true)
  | _ -> false

let parse_decl (j:json) : Decl.t option j_result =
  let open Rjson in
  let* o = cast_object j in
  if is_invalid o then Ok None
  else (
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    let inner = List.assoc_opt "inner" o |> Option.value ~default:(`List []) in
    let* inner = cast_list inner in
    let inner = List.filter is_valid_j inner in
    let attrs, inits = List.partition (fun j ->
      (
        let* o = cast_object j in
        let* k = get_kind o in
        Ok (match k with
          | "CUDASharedAttr" | "CUDADeviceAttr" -> true
          | _ -> false
        )
      ) |> (Result.value ~default:false)
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
    let ty_var = Ty_variable.make ~name ~ty:(J_type.from_json ty) in
    Ok (Some (Decl.make ~ty_var ~init ~attrs))
  )

let parse_for_init (j:json) : ForInit.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* kind = get_kind o in
  match kind with
  | "DeclStmt" ->
    let* ds = with_field "inner" (cast_map parse_decl) o in
    Ok (ForInit.Decls (Common.flatten_opt ds))
  | _ ->
    let* e = parse_exp j in
    Ok (ForInit.Expr e)

let rec parse_stmt (j:json) : Stmt.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let open Expr in
  let open Stmt in
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
      Result.value ~default:false has_typedecl
    in
    if has_typedecl then Ok (CompoundStmt []) else
    let static_assert : Stmt.t option =
      o
      |> with_field "inner" (cast_list_1 (fun j ->
          (* Ensure the expected kind *)
          let* o = cast_object j in
          let* _ = expect_kind "StaticAssertDecl" o in
          let* args = with_field "inner" (cast_map parse_exp) o in
          let static_assert : Decl_expr.t = {
            name = Variable.from_name "static_assert";
            ty=J_type.void;
            kind=Decl_expr.Kind.Function
          } in
          let func = Ident static_assert in
          Ok (SExpr (CallExpr {func;args;ty=J_type.void}))
        )
      )
      |> Result.to_option
    in
      (match static_assert with
        | Some e -> Ok e
        | None ->
          let* children = with_field "inner" (cast_map parse_decl) o in
          Ok (DeclStmt (children |> Common.flatten_opt))
      )
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
    let* children : Stmt.t list = with_field_or "inner" (fun (i:json) ->
      match i with
      | `Assoc _ -> let* o = parse_stmt i in Ok [o]
      | _ -> parse_stmt_list i
    ) [] o in
    Ok (CompoundStmt children)
  | Some "LabelStmt" ->
    (* TODO: do not parse LabelStmt *) 
    with_field "inner" (cast_list_1 parse_stmt) o
  | Some "ReturnStmt" ->
    let* e = with_field_or "inner" (
      cast_list_1 (fun j -> parse_exp j |> Result.map Option.some)
      ) None o
    in
    Ok (ReturnStmt e)
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
    Ok (SExpr e)
  | None -> Ok (CompoundStmt [])

and parse_stmt_list = fun inner ->
  let open Rjson in
  cast_list inner
  >>= map_all parse_stmt
    (fun idx s e -> StackTrace.Because (("error parsing statement #" ^ string_of_int (idx + 1), s), e))

let parse_param (j:json) : Param.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* name = parse_variable j in
  let* ty = get_field "type" o in
  let* is_refed = with_field_or "isReferenced" cast_bool false o in
  let* is_used =  with_field_or "isUsed" cast_bool false o in
  let* is_shared = with_field_or "shared" cast_bool false o in

  let ty_var : Ty_variable.t = Ty_variable.make ~ty:(J_type.from_json ty) ~name in
  Ok (Param.make
    ~is_used:(is_refed || is_used)
    ~ty_var:ty_var
    ~is_shared
  )

let j_filter_kind (f:string -> bool) (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let res =
    let* o = cast_object j in
    let* k = get_kind o in
    Ok (f k)
  in
  res |> Result.value ~default:false

let wrap_error (msg:string) (j:Yojson.Basic.t): 'a j_result -> 'a j_result =
    function
    | Ok e -> Ok e
    | Error e -> Rjson.because msg j e


let parse_kernel (type_params:Ty_param.t list) (j:Yojson.Basic.t) : Kernel.t j_result =
  let open Rjson in
  (
    let* o = cast_object j in
    let* ty = get_field "type" o |> Result.map J_type.from_json in
    let ty = J_type.to_string ty in
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
    let* body: Stmt.t list = parse_stmt_list (`List body) in
    let body = match body with
      | [s] -> s
      | _ -> CompoundStmt body
    in
    let* name: string = with_field "name" cast_string o in
    (* Parameters may be faulty, recover: *)
    let ps = List.map parse_param ps |> List.concat_map Result.to_list in
    Ok (Kernel.make
      ~ty
      ~name
      ~code:body
      ~params:ps
      ~type_params:type_params
      ~attribute:m
    )
  ) |> wrap_error "Kernel" j

(* Function that checks if a variable is of type array and is being used *)
let has_array_type (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let is_array =
    let* o = cast_object j in
    let* ty = get_field "type" o |> Result.map J_type.from_json in
    Ok (J_type.matches C_type.is_array ty)
  in
  is_array |> Result.value ~default:false


let is_kernel (j:Yojson.Basic.t) : bool =
  let open Rjson in
  let is_kernel =
    let* o = cast_object j in
    let* k = get_kind o in
    if k = "FunctionDecl" then (
      let* inner = with_field "inner" cast_list o in
      let attrs, inner =
        inner
        |> List.partition (j_filter_kind (String.ends_with ~suffix:"Attr"))
      in
      (* Try to parse attrs *)
      let attrs = attrs
        |> List.filter_map (fun j ->
          parse_attr j >>= (fun a -> Ok (Some a))
          |> Result.value ~default:None
        )
      in
      let params, _ =
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
  is_kernel |> Result.value ~default:false

let parse_type_param (j:Yojson.Basic.t) : Ty_param.t option j_result =
  let open Rjson in
  let* o = cast_object j in
  let* k = get_kind o in
  match k with
  | "TemplateTypeParmDecl" ->
    let* name = parse_variable j in
    Ok (Some (Ty_param.TemplateType name))
  | "NonTypeTemplateParmDecl" ->
    let* name = parse_variable j in
    let* ty = get_field "type" o in
    Ok (Some (Ty_param.NonTypeTemplate {name=name; ty=J_type.from_json ty}))
  | _ -> Ok None

let parse_constant (j:Yojson.Basic.t) : Enum.Constant.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* _ = expect_kind "EnumConstantDecl" o in
  let* var = parse_variable j in
  let* init = with_field_or "inner"
    (cast_list_1
      (fun j ->
        let* e = parse_exp j in
        match e with
        | IntegerLiteral n -> Ok (Some n)
        | _ -> root_cause ("Expecting an integer, but got something else") j
      )
    ) None o
  in
  let open Enum.Constant in
  Ok {var; init}

let parse_enum (j:Yojson.Basic.t) : Enum.t j_result =
  let open Rjson in
  let open Enum in
  let* var = parse_variable j in
  let* o = cast_object j in
  let* constants = with_field_or "inner" (cast_map parse_constant) [] o in
  Ok {var; constants}

let rec parse_def (j:Yojson.Basic.t) : Def.t list j_result =
  let open Rjson in
  let open Def in
  let* o = cast_object j in
  let* k = get_kind o in
  let parse_k (type_params:Ty_param.t list) (j:Yojson.Basic.t) : Def.t list j_result =
    if is_kernel j then (
      let* k = parse_kernel type_params j in
      (if k.code = CompoundStmt [] then Ok []
      else
        Ok [Kernel k])
    ) else Ok []
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
    let rec handle (type_params:Ty_param.t list): Yojson.Basic.t list -> Def.t list j_result =
      function
      | [] -> root_cause "Error parsing FunctionTemplateDecl: no FunctionDecl found" j
      | j :: l ->
        let* p = parse_type_param j in
        (match p with
        | Some p -> handle (p::type_params) l
        | None ->
          parse_k (List.rev type_params) j)
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
  | "TypedefDecl" ->
    let* name = with_field "name" cast_string o in
    let* ty = get_field "type" o |> Result.map J_type.from_json in

    (match J_type.to_c_type_res ty with
    | Ok ty ->
      if C_type.is_struct ty || C_type.is_array ty || C_type.is_function ty then
        Ok []
      else Ok [Typedef {name; ty;}]
    | Error _ -> Ok [])
  | "EnumDecl" ->
    let* e = parse_enum j in
    Ok [Enum e]
  | _ ->
    Ok []


(* ------------------------------------------------- *)


let parse_program ?(rewrite_shared_variables=true) (j:Yojson.Basic.t) : Program.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* inner = with_field "inner" (cast_map parse_def) o in
  let p = List.concat inner in
  Ok (if rewrite_shared_variables then Program.rewrite_shared_arrays p else p)

(* ------------------------------------------------------------------------ *)


