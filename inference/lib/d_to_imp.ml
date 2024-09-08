open Stage0
open Protocols
open Logger
open Imp

module StackTrace = Common.StackTrace
module KernelAttr = C_lang.KernelAttr
module StringMap = Common.StringMap

let (@) = Common.append_tr

open Exp

(* Monadic let *)
let (let*) = Result.bind
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

let parse_var: D_lang.Expr.t -> Variable.t d_result =
  function
  | Ident v -> Ok v.name
  | e -> root_cause ("parse_var: unexpected expression: " ^ D_lang.Expr.to_string e)

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

let rec parse_exp (e: D_lang.Expr.t) : Infer_exp.t d_result =
  let parse_e m e = with_exp m e parse_exp e in
  let ret_n (n:Infer_exp.n) : Infer_exp.t d_result = Ok (NExp n) in
  let ret_b (b:Infer_exp.b) : Infer_exp.t d_result = Ok (BExp b) in


  match e with
  (* ---------------- CUDA SPECIFIC ----------- *)
  | MemberExpr {base=Ident base; name=field; _} ->
    let v = base.name |> Variable.update_name (fun n -> n ^ "." ^ field) in
    ret_n (Var v)

  (* ------------------ nexp ------------------------ *)
  | Ident d ->
    ret_n (Var d.name)

  | SizeOfExpr ty ->
    (match J_type.to_c_type_res ty with
    | Ok ty ->
      let size = C_type.sizeof ty |> Option.value ~default:4 in
      L.warning ("sizeof(" ^ C_type.to_string ty ^ ") = " ^ string_of_int size);
      ret_n (Num size)
    | Error _ ->
      let lbl = "sizeof(" ^ J_type.to_string ty ^ ")" in
      L.warning ("could not parse type: " ^ lbl ^ " = ?");
      Ok (Unknown lbl))

  | IntegerLiteral n

  | CharacterLiteral n -> ret_n (Num n)

  | FloatingLiteral n -> 
    L.warning ("parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
    ret_n (Num (Float.to_int n))

  | ConditionalOperator o ->
    let* b = parse_e "cond" o.cond in
    let* n1 = parse_e "then_expr" o.then_expr in
    let* n2 = parse_e "else_expr" o.else_expr in
    ret_n (NIf (b, n1, n2))

  | UnaryOperator {opcode="~"; child=e; _} ->
    let* n = parse_e "child" e in
    ret_n (Unary (BitNot, n))

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _}
    when Variable.name n = "divUp" ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    (*  (n1 + n2 - 1)/n2 *)
    let n2_minus_1 : Infer_exp.n = Binary (Minus, n2, NExp (Num 1)) in
    let n1_plus_n2_minus_1 : Infer_exp.n = Binary (Plus, n1, NExp n2_minus_1) in
    ret_n (Binary (Div, NExp n1_plus_n2_minus_1, n2))

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__other_int" ->
    let* n = parse_e "arg" n in
    ret_n (Other n)

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__uniform_int" ->
    let* n = parse_e "arg" n in
    ret_b (Infer_exp.thread_equal n)

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__distinct_int" ->
    let* n = parse_e "arg" n in
    ret_b (Infer_exp.thread_distinct n)

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__is_pow2" ->
    let* n = parse_e "arg" n in
    ret_b (Pred ("pow2", n))

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _} when Variable.name n = "min" ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    ret_n (NIf (BExp (NRel (Lt, n1, n2)), n1, n2))

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _} when Variable.name n = "max" ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    ret_n (NIf (BExp (NRel (Gt, n1, n2)), n1, n2))

  | BinaryOperator {lhs=l; opcode="&"; rhs=IntegerLiteral 1; _} ->
    let* n = parse_exp l in
    ret_b (NRel (Eq, NExp (Binary (Mod, n, NExp (Num 2))), NExp (Num 0)))

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
    let* n = parse_e "arg" e in
    ret_b (Infer_exp.or_ (BExp (Pred ("pow2", n))) (BExp (Infer_exp.n_eq n (NExp (Num 0)))))

  | BinaryOperator {opcode=","; lhs=_; rhs=n; _} ->
    parse_e "comma-operator" n

  | BinaryOperator {opcode=o; lhs=n1; rhs=n2; _} ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    Ok (parse_bin o n1 n2)

  | CXXBoolLiteralExpr b ->
    ret_b (Bool b)

  | UnaryOperator u when u.opcode = "!" ->
    let* b = parse_e "not" u.child in
    ret_b (BNot b)

  | RecoveryExpr _
  | CXXConstructExpr _
  | MemberExpr _
  | CallExpr _ 
  | UnaryOperator _
  | CXXOperatorCallExpr _ ->
    let lbl = D_lang.Expr.to_string e in
    L.warning ("parse_exp: rewriting to unknown: " ^ lbl);
    Ok (Unknown lbl)

  | _ ->
    root_cause ("WARNING: parse_nexp: unsupported expression " ^ D_lang.Expr.name e ^ " : " ^ D_lang.Expr.to_string e)

let parse_type (e:J_type.t) : C_type.t d_result =
  e
  |> J_type.to_c_type_res
  |> Result.map_error (fun e ->
    Common.StackTrace.RootCause (Rjson.error_to_string e)
  )

module Arg = struct
  type i_array = {address: Variable.t; offset: Infer_exp.t}

  type t =
  | Scalar of Infer_exp.t
  | Array of i_array
  | Unsupported

  let rec parse_loc : D_lang.Expr.t -> i_array d_result =
    function
    | Ident v -> Ok {address=v.name; offset=NExp (Num 0)}
    | UnaryOperator {opcode; child=Ident _ as v; _} when opcode = "&" ->
      parse_loc v
    | BinaryOperator o when o.opcode = "+" ->
      let* lhs_ty = D_lang.Expr.to_type o.lhs |> parse_type in
      let address, offset =
        if C_type.is_array lhs_ty
        then o.lhs, o.rhs
        else o.rhs, o.lhs
      in
      let* l = with_msg "parse_loc.address" parse_loc address in
      let* offset = with_msg "parse_loc.offset"parse_exp offset in
      Ok {l with offset = NExp (Binary (Plus, offset, l.offset))}
    | e ->
      root_cause (
        "WARNING: parse_loc: unsupported expression: " ^ D_lang.Expr.to_string e
      )

  let parse (e: D_lang.Expr.t) : t d_result =
    let* ty = D_lang.Expr.to_type e |> parse_type in
    if C_type.is_array ty then (
      match parse_loc e with
      | Ok l -> Ok (Array l)
      | Error _ -> Ok Unsupported
    ) else if C_type.is_int ty then (
      (* Handle integer *)
      let* e = with_msg "Arg.parse" parse_exp e in
      Ok (Scalar e)
    ) else Ok Unsupported
end

module InferExp = struct
  let handle_arg (u:Infer_exp.Context.t) : Arg.t -> (Infer_exp.Context.t * Imp.Arg.t) =
    function
    | Scalar e ->
      let (u, e) = Infer_exp.handle_n u e in
      (u, Scalar e)
    | Array {offset; address} ->
      let (u, offset) = Infer_exp.handle_n u offset in
      (u, Array {address; offset})
    | Unsupported ->
      (u, Unsupported)

  let to_arg : Arg.t -> Variable.Set.t * Imp.Arg.t =
    Infer_exp.Context.convert handle_arg

  let to_arg_list : Arg.t list -> Variable.Set.t * Imp.Arg.t list =
    Infer_exp.Context.convert (Infer_exp.Context.mmap handle_arg)

  let ret_u (vars:Variable.Set.t) (s:Imp.Stmt.t) : Imp.Stmt.t list d_result =
    Ok (Infer_exp.decl_unknown vars @ [s])

  let ret_f ?(extra_vars=Variable.Set.empty) (f:'a -> Variable.Set.t * 'b) (handler:'b -> Imp.Stmt.t) (n:'a) : Imp.Stmt.t list d_result =
    let vars, n = f n in
    let vars = Variable.Set.union extra_vars vars in
    ret_u vars (handler n)

  let ret_n ?(extra_vars=Variable.Set.empty): (nexp -> Imp.Stmt.t) -> Infer_exp.t -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars Infer_exp.to_nexp

  let ret_ns ?(extra_vars=Variable.Set.empty): (nexp list -> Imp.Stmt.t) -> Infer_exp.t list -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars Infer_exp.to_nexp_list

  let ret_args ?(extra_vars=Variable.Set.empty): (Imp.Arg.t list -> Imp.Stmt.t) -> Arg.t list -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars to_arg_list

  let ret_b ?(extra_vars=Variable.Set.empty): (bexp -> Imp.Stmt.t) -> Infer_exp.t -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars Infer_exp.to_bexp
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

  let build_params (ps:(Variable.t * C_type.t) list) (ctx:t) : Params.t =
    List.fold_left (fun ps (x,ty) ->
      if is_enum ty ctx then
        Params.add_enum x (get_enum ty ctx) ps
      else
        Params.add x ty ps
    )
    Params.empty ps

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
  let gen_preamble (c:t) : Imp.Stmt.t list =
    let open Imp.Stmt in
    [Decl (List.map (fun (k,v) -> Imp.Decl.set k v) c.assigns)]
end

let cast_map f = Rjson.map_all f (fun idx _ e ->
  StackTrace.Because ("Error parsing list: error in index #" ^ (string_of_int (idx + 1)), e))

let parse_decl
  (ctx:Context.t)
  (d:D_lang.Decl.t)
:
  Imp.Decl.t list d_result
=
  let parse_e m b = with_msg (m ^ ": " ^ D_lang.Decl.to_string d) parse_exp b in
  let x = d.var in
  let ty =
    D_lang.Decl.types d
    |> List.map (fun ty ->
        Context.resolve (J_type.to_c_type ty) ctx
      )
    |> List.find_opt (fun ty ->
        Context.is_int ty ctx
      )
  in
  match ty with
  | Some ty ->
    let* ((vars, init):(Variable.Set.t * (nexp option))) =
      match d.init with
      | Some (IExpr n) ->
        let* n = parse_e "init" n in
        let (vars, n) = Infer_exp.to_nexp n in
        Ok (vars, Some n)
      | _ -> Ok (Variable.Set.empty, None)
    in
    let d =
      match init with
      | Some n -> Imp.Decl.set ~ty x n
      | None -> Imp.Decl.unset ~ty x
    in
    Ok (Infer_exp.as_decls vars @ [d])
  | None ->
    let x = Variable.name x in
    let ty = J_type.to_string d.ty in
    L.warning (
      "parse_decl: skipping non-int local variable '" ^ x ^ "' "^
      "type: " ^ ty
    );
    Ok []

let rec parse_load_expr (target:D_lang.Expr.t) (exp:D_lang.Expr.t)
  : (d_location_alias, D_lang.Expr.t) Either.t =
  let open Either in
  match exp with
  | Ident {ty; _} when J_type.matches C_type.is_pointer ty || J_type.matches C_type.is_array ty->
    Left {target=target; source=exp; offset=IntegerLiteral 0}
  | CXXOperatorCallExpr {func=UnresolvedLookupExpr {name=n; _}; args=[lhs;rhs]; ty}
  | CXXOperatorCallExpr {func=Ident {name=n; _}; args=[lhs;rhs]; ty}
    when Variable.name n = "operator+" ->
    (match parse_load_expr target lhs with
    | Left l ->
      let offset : D_lang.Expr.t = BinaryOperator {
        opcode = "+";
        lhs = l.offset;
        rhs = rhs;
        ty;
      } in
      Left {l with offset}
    | Right _ -> Right exp)
  | CXXOperatorCallExpr _ ->
    Right exp
  | BinaryOperator ({lhs=l; _} as b) ->
    (match parse_load_expr target l with
    | Left l ->
      let offset : D_lang.Expr.t = BinaryOperator {b with lhs=l.offset} in
      Left {l with offset}
    | Right _ -> Right exp)
  | _ ->
    Right exp



let parse_location_alias (s:d_location_alias) : Imp.Stmt.t list d_result =
  let* source = with_msg "location_alias.source" parse_var s.source in
  let* target = with_msg "location_alias.target" parse_var s.target in
  let* offset = with_msg "location_alias.offset" parse_exp s.offset in
  offset |> InferExp.ret_n (fun offset ->
    LocationAlias { target; source; offset; }
  )

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
    let (let*) = Option.bind in
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

  let parse_unop (u:'a Loop_infer.unop) : 'a unop option d_result =
    let* arg = parse_exp u.arg in
    Ok (match Infer_exp.try_to_nexp arg with
      | Some arg -> Some {op=u.op; arg=arg}
      | None -> None)

  let from_loop_infer (r:Loop_infer.t) : t option d_result =
    let* init = parse_exp r.init in
    let* cond = parse_unop r.cond in
    let* inc = parse_unop r.inc in
    Ok (match Infer_exp.try_to_nexp init, cond, inc with
    | Some init, Some cond, Some inc ->
      Some {name = r.name; init=init; cond=cond; inc=inc}
    | _, _, _ -> None)

end

let infer_for (r:D_lang.Stmt.d_for) : Range.t option d_result =
  match Loop_infer.from_for r with
  | Some r ->
    let* r = ForRange.from_loop_infer r in
    Ok (Option.bind r ForRange.to_range)
  | None -> Ok None

let infer_while (r:D_lang.Stmt.d_cond) : (Range.t * D_lang.Stmt.t) option d_result =
  match Loop_infer.from_while r with
  | Some (r, b) ->
    let* r = ForRange.from_loop_infer r in
    Ok (Option.bind r ForRange.to_range |> Option.map (fun r -> (r, b)))
  | None -> Ok None

let ret_loop (b:Imp.Stmt.t list) : Imp.Stmt.t list d_result =
  Ok [Imp.Stmt.Star (Block b)]

let ret (s:Imp.Stmt.t) : Imp.Stmt.t list d_result = Ok [s]

let ret_skip : Imp.Stmt.t list d_result = Ok []

let ret_assert (b:D_lang.Expr.t) (v:Imp.Assert.Visibility.t) : Imp.Stmt.t list d_result =
  let* b = with_msg "cond" parse_exp b in
  match Infer_exp.try_to_bexp b with
  | Some b -> ret (Assert (Imp.Assert.make b v))
  | None -> ret_skip

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
  Imp.Stmt.t list d_result
=
  let parse_stmt = parse_stmt ctx in
  let parse_decl = parse_decl ctx in
  let with_msg (m:string) f b = with_msg_ex (fun _ -> "parse_stmt: " ^ m ^ ": " ^ D_lang.Stmt.summarize c) f b in
  let ret_n = InferExp.ret_n in
  let ret_b = InferExp.ret_b in
  let ret_ns = InferExp.ret_ns in
  let ret_args = InferExp.ret_args in
  let resolve ty = Context.resolve ty ctx in

  match c with

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args=[]; _})
    when Variable.name n = "__syncthreads" ->
    ret (Sync n.location)

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args=[_]; _})
    when Variable.name n = "sync" ->
    ret (Sync n.location)

    (* Static assert may have a message as second argument *)
  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args = b :: _; _})
    when Variable.Set.mem n asserts ->
    ret_assert b Global

  | SExpr (CallExpr {func = f; args; _ }) as e ->
    let arg_count = List.length args in
    (match Context.lookup_sig f arg_count ctx with
    | Some s ->
      if List.length s.params = arg_count then (
        let* args = with_msg "call.args" (cast_map Arg.parse) args in
        args |> ret_args (fun args ->
          let args = List.map2 (fun x y -> (x, y)) s.params args in
          Imp.Stmt.Call {kernel=s.kernel; ty=s.ty; args}
        )
      ) else
        root_cause ("parse_stmt: CallExpr args mismatch: " ^ D_lang.Stmt.summarize e)
    | None ->
      Ok []
    )

  | DeclStmt ([{init=Some (IExpr (CallExpr {func = f; args; _ }) ); _}] as l) as d ->
    let arg_count = List.length args in
    (match Context.lookup_sig f arg_count ctx with
    | Some s ->
      if List.length s.params = arg_count then (
        let* args = with_msg "call.args" (cast_map Arg.parse) args in
        let r =
          args
          |> ret_args (fun args ->
            let args = List.map2 (fun x y -> (x, y)) s.params args in
            Imp.Stmt.Call {kernel=s.kernel; ty=s.ty; args}
          )
        in
        let* l = cast_map parse_decl l |> Result.map List.concat in
        r
        |> Result.map (fun s ->
          Imp.Stmt.Decl l :: s
        )
      ) else (
        let expecting = List.length s.params |> string_of_int in
        let given = List.length args |> string_of_int in
        root_cause (
          "parse_stmt: function call argument mismatch: " ^
          "signature has " ^ expecting ^ " parameters " ^
          D_lang.SignatureDB.Signature.to_string s ^ " " ^
          "but got " ^ given ^ " arguments: " ^ D_lang.Stmt.summarize d
        )
      )
    | None ->
      let* l = cast_map parse_decl l |> Result.map List.concat in
      ret (Decl l)
    )

  | WriteAccessStmt w ->
    let x = w.target.name |> Variable.set_location w.target.location in
    let* idx = with_msg "write.idx" (cast_map parse_exp) w.target.index in
    idx |> ret_ns (fun idx ->
      Write {array=x; index=idx; payload=w.payload}
    )

  | ReadAccessStmt r ->
    let array = r.source.name |> Variable.set_location r.source.location in
    let* idx = with_msg "read.idx" (cast_map parse_exp) r.source.index in
    let ty = r.ty |> resolve |> C_type.strip_array in
    idx
    |> ret_ns (fun index ->
      Read {target=r.target; array; index; ty}
    )

  | AtomicAccessStmt r ->
    let x = r.source.name |> Variable.set_location r.source.location in
    let* idx = with_msg "atomic.idx" (cast_map parse_exp) r.source.index in
    let ty = r.ty |> resolve |> C_type.strip_array in
    idx
    |> ret_ns (fun index ->
      Atomic {
        target=r.target;
        array=x;
        index;
        atomic=r.atomic;
        ty
      }
    )

  | IfStmt {cond=b;then_stmt=CompoundStmt[ReturnStmt None];else_stmt=CompoundStmt[]}
  | IfStmt {cond=b;then_stmt=ReturnStmt None;else_stmt=CompoundStmt[]} ->
    let ty = D_lang.Expr.to_type b in
    ret_assert (UnaryOperator {opcode="!"; child=b; ty}) Local

  | IfStmt {cond=b;then_stmt=CompoundStmt[BreakStmt];else_stmt=CompoundStmt[]}
  | IfStmt {cond=b;then_stmt=BreakStmt;else_stmt=CompoundStmt[]} ->
    let ty = D_lang.Expr.to_type b in
    ret_assert (UnaryOperator {opcode="!"; child=b; ty}) Local

  | IfStmt c ->
    let* b = with_msg "if.cond" parse_exp c.cond in
    let* t = with_msg "if.then" parse_stmt c.then_stmt in
    let* e = with_msg "if.else" parse_stmt c.else_stmt in
    b |> ret_b (fun b -> Imp.Stmt.s_if b (Block t) (Block e))

  | CompoundStmt l ->
    let* l = with_msg "block" (cast_map parse_stmt) l in
    ret (Block (List.flatten l))

  (* Support for location aliasing that declares a new variable *)
  | DeclStmt ([{ty; init=Some (IExpr rhs); _} as d] as l)
    (* either a pointer or an auto because templates *)
    when J_type.matches (fun x -> C_type.is_pointer x || C_type.is_auto x) ty
    ->
    let d_ty =
      d.ty
      |> J_type.to_c_type ~default:C_type.int
      |> resolve
      |> J_type.from_c_type
    in
    let lhs : D_lang.Expr.t = Ident (Decl_expr.from_name ~ty:d_ty d.var) in
    (match parse_load_expr lhs rhs with
    | Left a ->
      parse_location_alias a
    | Right _ ->
      (* fall back to the default parsing of decls *)
      let* l = cast_map parse_decl l |> Result.map List.concat in
      ret (Decl l)
    )

  | DeclStmt l ->
    let* l = cast_map parse_decl l |> Result.map List.concat in
    ret (Decl l)

  | SExpr ((BinaryOperator {opcode="="; lhs=Ident {ty; _} as lhs; rhs=rhs; _}))
    when J_type.matches C_type.is_pointer ty
    ->
    (match parse_load_expr lhs rhs with
    | Left a ->
      parse_location_alias a
    | Right _ -> Ok [])

  | SExpr (BinaryOperator {opcode="="; lhs=Ident {name=v; _}; rhs=rhs; ty; _})
    ->
    let* rhs = with_msg "assign.rhs" parse_exp rhs in
    let ty = J_type.to_c_type ~default:C_type.int ty |> resolve in
    rhs |> ret_n (fun rhs ->
      Imp.Stmt.assign ty v rhs
    )

  | ContinueStmt
  | BreakStmt
  | GotoStmt
  | ReturnStmt _
  | SExpr _ -> Ok []

  | ForStmt s ->
    let* r = infer_for s in
    let* b = with_msg "for.body" parse_stmt s.body in
    (match r with
    | Some r -> ret (For (r, Block b))
    | None -> ret_loop b)

  | DoStmt {body=body; _} ->
    let* body = with_msg "do.body" parse_stmt body in
    ret_loop body

  | WhileStmt w ->
    let* o = infer_while w in
    (match o with
    | Some (r, b) ->
      let* b = with_msg "while.body" parse_stmt b in
      ret (For (r, Block b))
    | None ->
      let* b = with_msg "while.body" parse_stmt w.body in
      ret_loop b
    )

  | SwitchStmt s ->
    with_msg "switch.body" parse_stmt s.body

  | CaseStmt s ->
    with_msg "case.body" parse_stmt s.body

  | DefaultStmt s ->
    with_msg "default.body" parse_stmt s

type param = (Variable.t * C_type.t, Variable.t * Memory.t) Either.t

let from_j_error (e:Rjson.j_error) : d_error =
  RootCause (Rjson.error_to_string e)


let parse_param
  (ctx:Context.t)
  (p:Param.t)
:
  param option d_result
=
  let mk_array (h:Mem_hierarchy.t) (ty:C_type.t) : Memory.t =
    {
      hierarchy = h;
      size = C_type.get_array_length ty;
      data_type = C_type.get_array_type ty;
    }
  in
  let* ty =
    p.ty_var.ty
    |> J_type.to_c_type_res
    |> Result.map_error from_j_error
    |> Result.map (fun x -> Context.resolve x ctx)
  in
  if Context.is_int ty ctx then
    let x = p.ty_var.name in
    Ok (Some (Either.Left (x, ty)))
  else if C_type.is_array ty then (
    let h =
      if p.is_shared then
        Mem_hierarchy.SharedMemory
      else
        Mem_hierarchy.GlobalMemory
    in
    Ok (Some (Either.Right (p.ty_var.name, mk_array h ty)))
  ) else Ok None


let parse_params
  (ctx:Context.t)
  (ps:Param.t list)
:
  (Params.t * Memory.t Variable.Map.t) d_result
=
  let* params = Rjson.map_all (parse_param ctx)
    (fun i _ e -> StackTrace.Because ("Error in index #" ^ string_of_int i, e)) ps in
  let globals, arrays = Common.flatten_opt params |> Common.either_split in
  Ok (Context.build_params globals ctx, Variable.Map.of_list arrays)

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
      -> arrays
    | IfStmt {then_stmt=s1; else_stmt=s2; _} ->
      let arrays = find_shared arrays s1 in
      find_shared arrays s2
    | CompoundStmt l ->
      List.fold_left find_shared arrays l
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
  (Context.t * Imp.Kernel.t) d_result
=
  let* code = parse_stmt ctx k.code in
  (* Add inferred arrays to global context *)
  let ctx =
    List.fold_left (fun ctx (x, m) ->
      Context.add_array x m ctx
    ) ctx (parse_shared k.code)
  in
  let* (params, arrays) = parse_params ctx k.params in
  let arrays = Variable.Map.union (fun _ _ r -> Some r) arrays ctx.arrays in
  let params = Params.union_right ctx.globals params in

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
  let open Imp.Stmt in
  let code = Block (Context.gen_preamble ctx @ code) in
  let params = add_type_params params k.type_params in
  let open Imp.Kernel in
  Ok (ctx, {
    name = k.name;
    ty = k.ty;
    code = code;
    params;
    arrays;
    visibility =
      match k.attribute with
      | Default -> Global
      | Auxiliary -> Device
    ;
  })

let parse_program (p:D_lang.Program.t) : Imp.Kernel.t list d_result =
  let rec parse_p
    (ctx:Context.t)
    (p:D_lang.Program.t)
  :
    Imp.Kernel.t list d_result
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
            | Some (IExpr n) ->
              (match parse_exp n with
              | Ok n -> Infer_exp.try_to_nexp n
              | Error _ -> None)
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
      let* (ctx, k) = parse_kernel ctx k in
      let* ks = parse_p ctx l in
      Ok (k::ks)
    | Typedef d :: l ->
      parse_p (Context.add_typedef d ctx) l
    | Enum e :: l ->
      parse_p (Context.add_enum e ctx) l
    | [] -> Ok []
  in
  let sigs = D_lang.SignatureDB.from_program p in
  parse_p (Context.from_signature_db sigs) p
end

module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
