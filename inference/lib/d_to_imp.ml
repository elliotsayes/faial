open Stage0
open Protocols
open Logger
open Imp

module StackTrace = Stack_trace
module KernelAttr = C_lang.KernelAttr
module StringMap = Common.StringMap
module Param = C_lang.Param
module Ty_param = C_lang.Ty_param

let (@) = Common.append_tr

open Exp

(* Monadic let *)
let ( let* ) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

type d_error = string StackTrace.t

let print_error : d_error -> unit =
  StackTrace.iter prerr_endline

let error_to_buffer (e: d_error) : Buffer.t =
  let b = Buffer.create 512 in
  StackTrace.iter (Buffer.add_string b) e;
  b

type 'a d_result = ('a, d_error) Result.t

let unwrap : 'a d_result -> 'a =
  function
  | Ok a -> a
  | Error e -> failwith (error_to_buffer e |> Buffer.contents)

let root_cause (msg:string) : 'a d_result =
  Error (RootCause msg)

let with_msg_ex (on_err:'a -> string) (f:'a -> 'b d_result) (c:'a): 'b d_result =
  match f c with
  | Ok o -> Ok o
  | Error err -> Error (Because (on_err c, err))

let with_msg (msg:string) (f:'a -> 'b d_result) (c:'a): 'b d_result =
  match f c with
  | Ok o -> Ok o
  | Error err -> Error (Because (msg, err))

let with_exp (msg:string) (e: D_lang.Expr.t) : (D_lang.Expr.t -> 'a d_result) -> D_lang.Expr.t -> 'a d_result =
  with_msg (msg ^ ": " ^ D_lang.Expr.to_string e)

let parse_var: D_lang.Expr.t -> Variable.t =
  function
  | Ident v -> v.name
  | e -> failwith ("parse_var: unexpected expression: " ^ D_lang.Expr.to_string e)

type d_access = {location: Variable.t; mode: Access.Mode.t; index: D_lang.Expr.t list }

type d_location_alias = {
  source: D_lang.Expr.t;
  target: D_lang.Expr.t;
  offset: D_lang.Expr.t;
}

module TypeAlias = struct
  type t = C_type.t StringMap.t
  let empty : t = StringMap.empty

  (* Resolve a type according to the alias in the database *)
  let resolve (ty:C_type.t) (db:t) : C_type.t =
    StringMap.find_opt (C_type.to_string ty) db
    |> Option.value ~default:ty

  (* Add a new type alias to the data-base *)
  let add (x:Typedef.t) (db:t) : t =
    (* Resolve the type so that there are no indirect alias *)
    StringMap.add x.name (resolve x.ty db) db

end

module Make (L: Logger) = struct

let parse_bin (op:string) (l:Imp.Infer_exp.t) (r:Infer_exp.t) : Infer_exp.t =
  match op with
  (* bool -> bool -> bool *)
  | "||" -> BExp (BRel (BOr, l, r))
  | "&&" -> BExp (BRel (BAnd, l, r))
  (* int -> int -> bool *) 
  | "==" -> BExp (NRel (Eq, l, r))
  | "!=" -> BExp (NRel (Neq, l, r))
  | "<=" -> BExp (NRel (Le, l, r))
  | "<"  -> BExp (NRel (Lt, l, r))
  | ">=" -> BExp (NRel (Ge, l, r))
  | ">"  -> BExp (NRel (Gt, l, r))
  (* int -> int -> int *)
  | "+" -> NExp (Binary (Plus, l, r))
  | "-" -> NExp (Binary (Minus, l, r))
  | "*"  -> NExp (Binary (Mult, l, r))
  | "/" -> NExp (Binary (Div, l, r))
  | "%" -> NExp (Binary (Mod, l, r))
  | ">>" -> NExp (Binary (RightShift, l, r))
  | "<<" -> NExp (Binary (LeftShift, l, r))
  | "^" -> NExp (Binary (BitXOr, l, r))
  | "|" -> NExp (Binary (BitOr, l, r))
  | "&" -> NExp (Binary (BitAnd, l, r))
  | _ ->
    L.warning ("parse_bin: rewriting to unknown binary operator: " ^ op);
    let lbl =
    "(" ^ Infer_exp.to_string l ^ ") " ^ op ^ " " ^
    "(" ^ Infer_exp.to_string r ^ ")" in
    Unknown lbl

let rec parse_exp (e:D_lang.Expr.t) : Infer_exp.t =
  match e with
  (* ---------------- CUDA SPECIFIC ----------- *)
  | MemberExpr {base=Ident base; name=field; _} ->
    let v = base.name |> Variable.update_name (fun n -> n ^ "." ^ field) in
    NExp (Var v)

  (* ------------------ nexp ------------------------ *)
  | Ident d ->
    NExp (Var d.name)

  | SizeOfExpr ty ->
    (match J_type.to_c_type_res ty with
    | Ok ty ->
      let size = C_type.sizeof ty |> Option.value ~default:4 in
      L.warning ("sizeof(" ^ C_type.to_string ty ^ ") = " ^ string_of_int size);
      NExp (Num size)
    | Error _ ->
      let lbl = "sizeof(" ^ J_type.to_string ty ^ ")" in
      L.warning ("could not parse type: " ^ lbl ^ " = ?");
      Unknown lbl)

  | IntegerLiteral n

  | CharacterLiteral n -> NExp (Num n)

  | FloatingLiteral n -> 
    L.warning ("parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
    NExp (Num (Float.to_int n))

  | ConditionalOperator o ->
    let b = parse_exp o.cond in
    let n1 = parse_exp o.then_expr in
    let n2 = parse_exp o.else_expr in
    NExp (NIf (b, n1, n2))

  | UnaryOperator {opcode="~"; child=e; _} ->
    let n = parse_exp e in
    NExp (Unary (BitNot, n))

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _}
    when Variable.name n = "divUp" ->
    let n1 = parse_exp n1 in
    let n2 = parse_exp n2 in
    (*  (n1 + n2 - 1)/n2 *)
    let n2_minus_1 : Infer_exp.n = Binary (Minus, n2, NExp (Num 1)) in
    let n1_plus_n2_minus_1 : Infer_exp.n = Binary (Plus, n1, NExp n2_minus_1) in
    NExp (Binary (Div, NExp n1_plus_n2_minus_1, n2))

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__other_int" ->
    let n = parse_exp n in
    NExp (Other n)

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__uniform_int" ->
    let n = parse_exp n in
    BExp (Infer_exp.thread_equal n)

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__distinct_int" ->
    let n = parse_exp n in
    BExp (Infer_exp.thread_distinct n)

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__is_pow2" ->
    let n = parse_exp n in
    BExp (Pred ("pow2", n))

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _} when Variable.name n = "min" ->
    let n1 = parse_exp n1 in
    let n2 = parse_exp n2 in
    NExp (NIf (BExp (NRel (Lt, n1, n2)), n1, n2))

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _} when Variable.name n = "max" ->
    let n1 = parse_exp n1 in
    let n2 = parse_exp n2 in
    NExp (NIf (BExp (NRel (Gt, n1, n2)), n1, n2))

  | BinaryOperator {lhs=l; opcode="&"; rhs=IntegerLiteral 1; _} ->
    let n = parse_exp l in
    BExp (NRel (Eq, NExp (Binary (Mod, n, NExp (Num 2))), NExp (Num 0)))

  | BinaryOperator {
      opcode="==";
      lhs=BinaryOperator {
        opcode="&";
        lhs=Ident n1 as e;
        rhs=BinaryOperator {
          opcode="-";
          lhs=Ident n2;
          rhs=IntegerLiteral 1; _
        }; _
      };
      rhs=IntegerLiteral 0;
      _
    } when Decl_expr.equal n1 n2 ->
    let n = parse_exp e in
    BExp (Infer_exp.or_ (BExp (Pred ("pow2", n))) (BExp (Infer_exp.n_eq n (NExp (Num 0)))))

  | BinaryOperator {opcode=","; lhs=_; rhs=e; _} ->
    print_endline "!!!!!!";
    parse_exp e

  | BinaryOperator {opcode=o; lhs=n1; rhs=n2; _} ->
    let n1 = parse_exp n1 in
    let n2 = parse_exp n2 in
    parse_bin o n1 n2

  | CXXBoolLiteralExpr b ->
    BExp (Bool b)

  | UnaryOperator u when u.opcode = "!" ->
    let b = parse_exp u.child in
    BExp (BNot b)

  | RecoveryExpr _
  | CXXConstructExpr _
  | MemberExpr _
  | CallExpr _ 
  | UnaryOperator _
  | CXXOperatorCallExpr _ ->
    let lbl = D_lang.Expr.to_string e in
    L.warning ("parse_exp: rewriting to unknown: " ^ lbl);
    Unknown lbl

  | _ ->
    failwith ("WARNING: parse_nexp: unsupported expression " ^ D_lang.Expr.name e ^ " : " ^ D_lang.Expr.to_string e)

let to_nexp (e:D_lang.Expr.t) : Exp.nexp Infer_exp.state =
  Infer_exp.to_nexp (parse_exp e)

let to_bexp (e:D_lang.Expr.t) : Exp.bexp Infer_exp.state =
  Infer_exp.to_bexp (parse_exp e)

let try_to_nexp (e:D_lang.Expr.t) : Exp.nexp option =
  e
  |> parse_exp
  |> Infer_exp.to_nexp
  |> Infer_exp.no_unknowns

let try_to_bexp (e:D_lang.Expr.t) : Exp.bexp option =
  e
  |> parse_exp
  |> Infer_exp.to_bexp
  |> Infer_exp.no_unknowns

let parse_type (e:J_type.t) : C_type.t =
  e
  |> J_type.to_c_type_res
  |> Result.map_error (fun e ->
    Stack_trace.RootCause (Rjson.error_to_string e)
  )
  |> unwrap


module Arg = struct
  open Imp
  open Stage0.State.Syntax

  let rec parse_array : D_lang.Expr.t -> Array_use.t option Infer_exp.state =
    function
    | Ident v ->
      return (Some (Array_use.make v.name))
    | UnaryOperator {opcode; child=Ident _ as v; _} when opcode = "&" ->
      parse_array v
    | BinaryOperator o when o.opcode = "+" ->
      let lhs_ty = D_lang.Expr.to_type o.lhs |> parse_type in
      let address, offset =
        if C_type.is_array lhs_ty
        then o.lhs, o.rhs
        else o.rhs, o.lhs
      in
      let* arr = parse_array address in
      (match arr with
      | Some arr ->
        let* offset = to_nexp offset in
        let arr : Imp.Array_use.t = {
          arr with offset = Exp.n_plus offset arr.offset
        } in
        return (Some arr)
      | None -> return None)
    | _ ->
      return None

  let parse (e: D_lang.Expr.t) : Arg.t Infer_exp.state =
    let open Imp.Arg in
    let ty = D_lang.Expr.to_type e |> parse_type in
    if C_type.is_array ty then (
      let* a = parse_array e in
      return (
        match a with
        | Some l -> Array l
        | None -> Unsupported
      )
    ) else if C_type.is_int ty then (
      (* Handle integer *)
      let* e = to_nexp e in
      return (Scalar e)
    ) else
      return Unsupported
end

(* -------------------------------------------------------------- *)

module Context = struct
  type t = {
    sigs: D_lang.SignatureDB.t;
    arrays: Memory.t Variable.Map.t;
    globals: Params.t;
    assigns: (Variable.t * nexp) list;
    typedefs: TypeAlias.t;
    enums: Enum.t Variable.Map.t;
  }

  let to_string (ctx:t) : string =
    [
      "sigs: " ^ D_lang.SignatureDB.to_string ctx.sigs;
      "arrays: [" ^ (ctx.arrays |> Variable.Map.to_list |> List.map fst |> List.map Variable.name |> String.concat ", ") ^ "]";
    ]
    |> String.concat "\n"

  let from_signature_db (sigs:D_lang.SignatureDB.t) : t =
    {
      sigs;
      arrays = Variable.Map.empty;
      globals = Params.empty;
      assigns = [];
      typedefs = TypeAlias.empty;
      enums = Variable.Map.empty;
    }

  let resolve (ty:C_type.t) (b:t) : C_type.t =
    TypeAlias.resolve ty b.typedefs

  let lookup_sig (e: D_lang.Expr.t) (arg_count:int) (db:t) : D_lang.SignatureDB.Signature.t option =
    D_lang.SignatureDB.lookup e arg_count db.sigs

  let is_enum (ty:C_type.t) (ctx:t) : bool =
    let name = C_type.to_string ty |> Variable.from_name in
    Variable.Map.mem name ctx.enums

  let is_int (ty:C_type.t) (ctx:t) : bool =
    C_type.is_int ty || is_enum ty ctx

  let get_enum (ty:C_type.t) (ctx:t) : Enum.t =
    let name = C_type.to_string ty |> Variable.from_name in
    Variable.Map.find name ctx.enums

  let add_array (var:Variable.t) (m:Memory.t) (b:t) : t =
    { b with arrays = Variable.Map.add var m b.arrays; }

  let add_assign (var:Variable.t) (n:Exp.nexp) (b:t) : t =
    { b with assigns = (var, n) :: b.assigns }

  let add_global (var:Variable.t) (ty:C_type.t) (b:t) : t =
    { b with globals = Params.add var ty b.globals }

  let add_typedef (d:Typedef.t) (b:t) : t =
    { b with typedefs = TypeAlias.add d b.typedefs }

  let add_enum (e:Enum.t) (b:t) : t =
    let assigns =
      if Enum.ignore e then
        b.assigns
      else
        Enum.to_assigns e @ b.assigns
    in
    { b with assigns; enums = Variable.Map.add e.var e b.enums;}

  (* Generate the preamble *)
  let gen_preamble (c:t) : Imp.Stmt.t =
    let open Imp.Stmt in
    [Decl (List.map (fun (k,v) -> Imp.Decl.set k v) c.assigns)]
    |> Stmt.from_list
end

open Stage0.State.Syntax

let parse_decl
  (ctx:Context.t)
  (d:D_lang.Decl.t)
:
  Imp.Decl.t option Imp.Infer_exp.state
=
  let x = d.var in
  match
    D_lang.Decl.types d
    |> List.map (fun ty ->
        Context.resolve (J_type.to_c_type ty) ctx
      )
    |> List.find_opt (fun ty ->
        Context.is_int ty ctx
      )
  with
  | Some ty ->
    let* init : nexp option =
      match d.init with
      | Some (IExpr n) ->
        let* n = to_nexp n in
        return (Some n)
      | _ -> return None
    in
    let d =
      match init with
      | Some n -> Imp.Decl.set ~ty x n
      | None -> Imp.Decl.unset ~ty x
    in
    return (Some d)
  | None ->
    let x = Variable.name x in
    let ty = J_type.to_string d.ty in
    L.warning (
      "parse_decl: skipping non-int local variable '" ^ x ^ "' "^
      "type: " ^ ty
    );
    return None

let parse_decl_stmt
  (ctx:Context.t)
  (d:D_lang.Decl.t)
:
  Imp.Stmt.t
=
  Infer_exp.unknowns (
    let* d = parse_decl ctx d in
    return (
      match d with
      | Some d -> Stmt.Decl [d]
      | None -> Stmt.Skip
    )
  )

let rec parse_load_expr
  (target:D_lang.Expr.t)
  (exp:D_lang.Expr.t)
:
  d_location_alias option
=
  let ( let* ) = Option.bind in
  match exp with
  | Ident {ty; _} when J_type.matches C_type.is_pointer ty || J_type.matches C_type.is_array ty->
    Some {target=target; source=exp; offset=IntegerLiteral 0}
  | CXXOperatorCallExpr {func=UnresolvedLookupExpr {name=n; _}; args=[lhs;rhs]; ty}
  | CXXOperatorCallExpr {func=Ident {name=n; _}; args=[lhs;rhs]; ty}
    when Variable.name n = "operator+" ->
    let* l = parse_load_expr target lhs in
    let offset : D_lang.Expr.t = BinaryOperator {
      opcode = "+";
      lhs = l.offset;
      rhs = rhs;
      ty;
    } in
    Some {l with offset}
  | CXXOperatorCallExpr _ ->
    None
  | BinaryOperator ({lhs=l; _} as b) ->
    let* l = parse_load_expr target l in
    let offset : D_lang.Expr.t = BinaryOperator {b with lhs=l.offset} in
    Some {l with offset}
  | _ ->
    None

let parse_location_alias (s:d_location_alias) : Imp.Stmt.t =
  Infer_exp.unknowns (
    let source = parse_var s.source in
    let target = parse_var s.target in
    let* offset = to_nexp s.offset in
    return (Imp.Stmt.LocationAlias { target; source; offset; })
  )


let cast_map f = Rjson.map_all f (fun idx _ e ->
  StackTrace.Because ("Error parsing list: error in index #" ^ (string_of_int (idx + 1)), e))

type 'a unop =
  {op: 'a; arg: nexp}

module ForRange = struct
  type t = {
    name: Variable.t;
    init: nexp;
    cond: Loop_infer.comparator unop;
    inc: Loop_infer.increment unop;
  }

  let infer_bounds : t -> nexp * nexp * Range.direction =
    function
    (* (int i = 0; i < 4; i++) *)
    | {init=lb; cond={op=Lt; arg=ub; _}; _} ->
      (lb, Binary (Minus, ub, Num 1), Range.Increase)
    (* (int i = 0; i <= 4; i++) *)
    | {init=lb; cond={op=LtEq; arg=ub; _}; _} ->
      (lb, ub, Increase)
    (* (int i = 4; i - k; i++) *)
    | {init=lb; cond={op=RelMinus; arg=ub; _}; _} ->
      (lb, ub, Range.Increase)
    (* (int i = 4; i >= 0; i--) *)
    | {init=ub; cond={op=GtEq; arg=lb; _}; _} ->
      (lb, ub, Decrease)
    (* (int i = 4; i > 0; i--) *)
    | {init=ub; cond={op=Gt; arg=lb; _}; _} ->
      (Binary (Plus, Num 1, lb), ub, Decrease)


  let infer_step (r:t) : Range.Step.t option =
    match r.inc with
    | {op=Plus; arg=a}
    | {op=Minus; arg=a} -> Some (Range.Step.Plus a)
    | {op=Mult; arg=a}
    | {op=Div; arg=a} ->
      Some (Range.Step.Mult a)
    | {op=LShift; arg=Num a}
    | {op=RShift; arg=Num a} ->
      Some (Range.Step.Mult (Num (Common.pow ~base:2 a)))
    | _ -> None

  let to_range (r:t) : Range.t option =
    let ( let* ) = Option.bind in
    let (lb, ub, d) = infer_bounds r in
    let* step = infer_step r in
    Some Range.{
      var=r.name;
      lower_bound=lb;
      upper_bound=ub;
      step=step;
      dir=d;
      ty=C_type.int;
    }

  let parse_unop (u:'a Loop_infer.unop) : 'a unop option =
    let arg = parse_exp u.arg in
    match Infer_exp.(no_unknowns (to_nexp arg)) with
    | Some arg -> Some {op=u.op; arg=arg}
    | None -> None

  let from_loop_infer (r:Loop_infer.t) : t option =
    let init = parse_exp r.init in
    let cond = parse_unop r.cond in
    let inc = parse_unop r.inc in
    match Infer_exp.(no_unknowns (to_nexp init)), cond, inc with
    | Some init, Some cond, Some inc ->
      Some {name = r.name; init=init; cond=cond; inc=inc}
    | _, _, _ -> None

end

let infer_for (r:D_lang.Stmt.d_for) : Range.t option =
  match Loop_infer.from_for r with
  | Some r ->
    let r = ForRange.from_loop_infer r in
    Option.bind r ForRange.to_range
  | None -> None

let infer_while (r:D_lang.Stmt.d_cond) : (Range.t * D_lang.Stmt.t) option =
  match Loop_infer.from_while r with
  | Some (r, b) ->
    let r = ForRange.from_loop_infer r in
    Option.bind r ForRange.to_range |> Option.map (fun r -> (r, b))
  | None -> None


let ret_assert (b:D_lang.Expr.t) (v:Imp.Assert.Visibility.t) : Imp.Stmt.t =
  let b = parse_exp b in
  let open Imp.Stmt in
  match Infer_exp.(no_unknowns (to_bexp b)) with
  | Some b -> Assert (Imp.Assert.make b v)
  | None -> Skip

let asserts : Variable.Set.t =
  Variable.Set.of_list [
    Variable.from_name "assert";
    Variable.from_name "static_assert";
    Variable.from_name "__requires"
  ]



let rec parse_stmt
  (ctx:Context.t)
  (c:D_lang.Stmt.t)
:
  Imp.Stmt.t
=
  let parse_stmt = parse_stmt ctx in
  let resolve ty = Context.resolve ty ctx in

  match c with
  | Skip -> Skip

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args=[]; _})
    when Variable.name n = "__syncthreads" ->
    Sync n.location

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args=[_]; _})
    when Variable.name n = "sync" ->
    Sync n.location

    (* Static assert may have a message as second argument *)
  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args = b :: _; _})
    when Variable.Set.mem n asserts ->
    ret_assert b Global

  | SExpr (CallExpr {func = f; args; _ }) as e ->
    let arg_count = List.length args in
    (match Context.lookup_sig f arg_count ctx with
    | Some s ->
      if List.length s.params <> arg_count then (
        failwith ("parse_stmt: CallExpr args mismatch: " ^ D_lang.Stmt.summarize e)
      ) else
      Infer_exp.unknowns (
        let* args = State.list_map Arg.parse args in
        let args = List.map2 (fun x y -> (x, y)) s.params args in
        let open Imp.Stmt in
        return (Call {kernel=s.kernel; ty=s.ty; args})
      )
    | None ->
      Skip
    )

  | WriteAccessStmt w ->
    let array =
      w.target.name |> Variable.set_location w.target.location
    in
    Infer_exp.unknowns (
      let* index = State.list_map to_nexp w.target.index in
      return (Stmt.Write {array; index; payload=w.payload})
    )

  | ReadAccessStmt r ->
    let array = r.source.name |> Variable.set_location r.source.location in
    Infer_exp.unknowns (
      let* index = State.list_map to_nexp r.source.index in
      let ty = r.ty |> resolve |> C_type.strip_array in
      return (Stmt.Read {target=Some (ty, r.target); array; index})
    )

  | AtomicAccessStmt r ->

    let array = r.source.name |> Variable.set_location r.source.location in
    Infer_exp.unknowns (
      let* index = State.list_map to_nexp r.source.index in
      let ty = r.ty |> resolve |> C_type.strip_array in
      return (Stmt.Atomic {
          target=r.target;
          atomic=r.atomic;
          array;
          index;
          ty
        }
      )
    )

  | IfStmt {cond=b;then_stmt=ReturnStmt None;else_stmt=Skip} ->
    let ty = D_lang.Expr.to_type b in
    ret_assert (UnaryOperator {opcode="!"; child=b; ty}) Local

  | IfStmt {cond=b;then_stmt=BreakStmt;else_stmt=Skip} ->
    let ty = D_lang.Expr.to_type b in
    ret_assert (UnaryOperator {opcode="!"; child=b; ty}) Local

  | IfStmt c ->
    Infer_exp.unknowns (
      let* b = to_bexp c.cond in
      let t = parse_stmt c.then_stmt in
      let e = parse_stmt c.else_stmt in
      return (Imp.Stmt.if_ b t e)
    )

  (* Support for location aliasing that declares a new variable *)
  | DeclStmt [d] ->
    let ( let* ) = Option.bind in
    (* Detect non-standard declarations: *)
    let s =
      match d with
      (* Detect kernel-calls: *)
      | {init=Some (IExpr rhs); _} when D_lang.Expr.is_call rhs ->
        (* Found a call, so extract the call and parse the rest
          of the declaration yet unsetting the first decl *)
        Some (
          Stmt.seq
            (parse_stmt (SExpr rhs))
            (parse_stmt (DeclStmt [{d with init = None}]))
        )
      (* Detect array alias: *)
      | {ty; init=Some (IExpr rhs); _} when
        J_type.matches (fun x -> C_type.is_pointer x || C_type.is_auto x) ty
      ->
        let d_ty =
          d.ty
          |> J_type.to_c_type ~default:C_type.int
          |> resolve
          |> J_type.from_c_type
        in
        let lhs : D_lang.Expr.t = Ident (Decl_expr.from_name ~ty:d_ty d.var) in
        let* a = parse_load_expr lhs rhs in
        Some (parse_location_alias a)
      (* Otherwise, nothing found *)
      | _ ->
        None
    in
    (match s with
    | Some s -> s
    | None ->
      (* fall back to the default parsing of decls *)
      parse_decl_stmt ctx d)
  | DeclStmt (d :: l) ->
    Stmt.seq (parse_stmt (DeclStmt [d])) (parse_stmt (DeclStmt l))
  | DeclStmt [] -> Skip

  | SExpr ((BinaryOperator {opcode="="; lhs=Ident {ty; _} as lhs; rhs=rhs; _}))
    when J_type.matches C_type.is_pointer ty
  ->
    parse_load_expr lhs rhs
    |> Option.map parse_location_alias
    |> Option.value ~default:Stmt.Skip

  | SExpr (BinaryOperator {opcode="="; lhs=Ident {name=v; _}; rhs=rhs; ty; _})
    ->
    Infer_exp.unknowns (
      let* rhs = to_nexp rhs in
      let ty = J_type.to_c_type ~default:C_type.int ty |> resolve in
      return (Stmt.assign ty v rhs)
    )

  | ContinueStmt
  | BreakStmt
  | GotoStmt
  | ReturnStmt _
  | SExpr _ -> Stmt.Skip

  | ForStmt s ->
    let b = parse_stmt s.body in
    (match infer_for s with
    | Some r -> For (r, b)
    | None -> Stmt.Star b)

  | DoStmt {body=body; _} ->
    let body = parse_stmt body in
    Stmt.Star body

  | WhileStmt w ->
    (match infer_while w with
    | Some (r, b) ->
      let b = parse_stmt b in
      For (r, b)
    | None ->
      let b = parse_stmt w.body in
      Stmt.Star b
    )

  | SwitchStmt {body=s; _}
  | CaseStmt {body=s; _}
  | DefaultStmt s ->
    parse_stmt s
  | Seq (s1, s2) ->
    Seq (parse_stmt s1, parse_stmt s2)

type param = (Variable.t * C_type.t, Variable.t * Memory.t) Either.t

let from_j_error (e:Rjson.j_error) : d_error =
  RootCause (Rjson.error_to_string e)


let parse_param
  (ctx:Context.t)
  (p:Param.t)
:
  Kernel.Parameter.t
=
  let mk_array (h:Mem_hierarchy.t) (ty:C_type.t) : Memory.t =
    {
      hierarchy = h;
      size = C_type.get_array_length ty;
      data_type = C_type.get_array_type ty;
    }
  in
  let ty =
    p.ty_var.ty
    |> J_type.to_c_type_res
    |> Result.map_error from_j_error
    |> Result.map (fun x -> Context.resolve x ctx)
    |> unwrap
  in
  let x = p.ty_var.name in
  if Context.is_enum ty ctx then
    Kernel.Parameter.enum x (Context.get_enum ty ctx)
  else if Context.is_int ty ctx then
    Kernel.Parameter.scalar x ty
  else if C_type.is_array ty then (
    let h =
      if p.is_shared then
        Mem_hierarchy.SharedMemory
      else
        Mem_hierarchy.GlobalMemory
    in
    Kernel.Parameter.array x (mk_array h ty)
  ) else
    Kernel.Parameter.unsupported x

let parse_shared (s:D_lang.Stmt.t) : (Variable.t * Memory.t) list =
  let open D_lang in
  let rec find_shared
    (arrays:(Variable.t * array_t) list)
    (s:Stmt.t)
  :
    (Variable.t * array_t) list
  =
    match s with
    | DeclStmt l ->
      List.filter_map (fun (d:Decl.t) ->
        Decl.get_shared d
        |> Option.map (fun a ->
          (d.var, a))
      ) l
      |> Common.append_tr arrays
    | WriteAccessStmt _
    | ReadAccessStmt _
    | AtomicAccessStmt _
    | GotoStmt
    | ReturnStmt _
    | ContinueStmt
    | BreakStmt
    | SExpr _
    | Skip
      -> arrays
    | Seq (s1, s2)
    | IfStmt {then_stmt=s1; else_stmt=s2; _} ->
      let arrays = find_shared arrays s1 in
      find_shared arrays s2
    | ForStmt {body=d; _}
    | WhileStmt {body=d; _}
    | DoStmt {body=d; _}
    | SwitchStmt {body=d; _}
    | DefaultStmt d
    | CaseStmt {body=d; _}
      -> find_shared arrays d
  in
  find_shared [] s

let parse_kernel
  (ctx:Context.t)
  (k:D_lang.Kernel.t)
:
  (Context.t * Imp.Kernel.t)
=
  let code = parse_stmt ctx k.code in
  (* Add inferred shared arrays to global context *)
  let ctx =
    List.fold_left (fun ctx (x, m) ->
      Context.add_array x m ctx
    ) ctx (parse_shared k.code)
  in
  (* Parse kernel parameters *)
  let parameters = List.map (parse_param ctx) k.params in
  (* type parameters become global variables because c-t-j doesn't represent
     type instantiations.
    *)
  let rec add_type_params (params:Params.t) : Ty_param.t list -> Params.t =
    function
    | [] -> params
    | TemplateType _ :: l -> add_type_params params l
    | NonTypeTemplate x :: l ->
      let params =
        match J_type.to_c_type_res x.ty with
        | Ok ty when C_type.is_int ty ->
          Params.add x.name ty params
        | _ -> params
      in
      add_type_params params l
  in
  let global_variables = add_type_params ctx.globals k.type_params in
  let open Imp.Stmt in
  let code = Seq (Context.gen_preamble ctx, code) in
  let open Imp.Kernel in
  (ctx, {
    name = k.name;
    ty = k.ty;
    code;
    parameters;
    global_arrays = ctx.arrays;
    global_variables;
    visibility =
      (match k.attribute with
        | Default -> Visibility.Global
        | Auxiliary -> Visibility.Device
      );
    block_dim = None;
    grid_dim = None;
  })

let parse_program (p:D_lang.Program.t) : Imp.Kernel.t list =
  let rec parse_p
    (ctx:Context.t)
    (p:D_lang.Program.t)
  :
    Imp.Kernel.t list
  =
    match p with
    | Declaration v :: l ->
      let b =
        match J_type.to_c_type_res v.ty with
        | Ok ty ->
          (* make sure we resolve the type before we query it *)
          let ty = Context.resolve ty ctx in
          let is_mut = not (C_type.is_const ty) in
          if is_mut && List.mem C_lang.c_attr_shared v.attrs then
            Context.add_array v.var (Memory.from_type SharedMemory ty) ctx
          else if is_mut && List.mem C_lang.c_attr_device v.attrs then
            Context.add_array v.var (Memory.from_type GlobalMemory ty) ctx
          else if Context.is_int ty ctx then
            let g = match v.init with
            | Some (IExpr n) -> try_to_nexp n
            | _ -> None
            in
            match g with
            | Some g -> Context.add_assign v.var g ctx
            | None -> Context.add_global v.var ty ctx
          else
            ctx
        | Error _ -> ctx
      in
      parse_p b l
    | Kernel k :: l ->
      let (ctx, k) = parse_kernel ctx k in
      let ks = parse_p ctx l in
      k::ks
    | Typedef d :: l ->
      parse_p (Context.add_typedef d ctx) l
    | Enum e :: l ->
      parse_p (Context.add_enum e ctx) l
    | [] -> []
  in
  let sigs = D_lang.SignatureDB.from_program p in
  parse_p (Context.from_signature_db sigs) p
end

module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
