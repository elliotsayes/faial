open Stage0
open Protocols
open Logger

module StackTrace = Common.StackTrace
module KernelAttr = C_lang.KernelAttr
module StringSet = Common.StringSet

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

let cuda_global_vars = [
  ("blockIdx", 0);
  ("blockDim", 1);
  ("gridDim", 1);
]

let cuda_local_vars = [("threadIdx", 0)]

let all_cuda_vars = cuda_local_vars @ cuda_global_vars

let cuda_base_vars : StringSet.t =
  all_cuda_vars
  |> List.map fst
  |> StringSet.of_list

let cuda_dims = ["x"; "y"; "z"]

type i_nexp =
  | Var of Variable.t
  | Num of int
  | Bin of nbin * i_exp * i_exp
  | NCall of string * i_exp
  | NIf of i_exp * i_exp * i_exp

and i_bexp =
  | Bool of bool
  | NRel of nrel * i_exp * i_exp
  | BRel of brel * i_exp * i_exp
  | BNot of i_exp
  | Pred of string * i_exp
  | ThreadEqual of i_exp

and i_exp =
  | NExp of i_nexp
  | BExp of i_bexp
  | Unknown

module Make (L: Logger) = struct

let parse_bin (op:string) (l:i_exp) (r:i_exp) : i_exp =
  match op with
  (* bool -> bool -> bool *)
  | "||" -> BExp (BRel (BOr, l, r))
  | "&&" -> BExp (BRel (BAnd, l, r))
  (* int -> int -> bool *) 
  | "==" -> BExp (NRel (NEq, l, r))
  | "!=" -> BExp (NRel (NNeq, l, r))
  | "<=" -> BExp (NRel (NLe, l, r))
  | "<"  -> BExp (NRel (NLt, l, r))
  | ">=" -> BExp (NRel (NGe, l, r))
  | ">"  -> BExp (NRel (NGt, l, r))
  (* int -> int -> int *)
  | "+" -> NExp (Bin (Plus, l, r))
  | "-" -> NExp (Bin (Minus, l, r))
  | "*"  -> NExp (Bin (Mult, l, r))
  | "/" -> NExp (Bin (Div, l, r))
  | "%" -> NExp (Bin (Mod, l, r))
  | ">>" -> NExp (Bin (RightShift, l, r))
  | "<<" -> NExp (Bin (LeftShift, l, r))
  | "^" -> NExp (Bin (BitXOr, l, r))
  | "|" -> NExp (Bin (BitOr, l, r))
  | "&" -> NExp (Bin (BitAnd, l, r))
  | _ ->
    L.warning ("parse_bin: rewriting to unknown binary operator: " ^ op);
    Unknown

let rec parse_exp (e: D_lang.Expr.t) : i_exp d_result =
  let parse_e m e = with_exp m e parse_exp e in
  let ret_n (n:i_nexp) : i_exp d_result = Ok (NExp n) in
  let ret_b (b:i_bexp) : i_exp d_result = Ok (BExp b) in


  match e with
  (* ---------------- CUDA SPECIFIC ----------- *)
  | MemberExpr {base=Ident base; name=field; _} ->
    let v = base.name |> Variable.update_name (fun n -> n ^ "." ^ field) in
    ret_n (Var v)

  (* ------------------ nexp ------------------------ *)
  | Ident d ->
    ret_n (Var d.name)
  | SizeOfExpr ty ->
    (match C_type.from_json ty with
    | Ok ty ->
      let size = C_type.sizeof ty |> Option.value ~default:4 in
      L.warning ("sizeof(" ^ C_type.to_string ty ^ ") = " ^ string_of_int size);
      ret_n (Num size)
    | Error _ ->
      L.warning ("could not parse type: sizeof(" ^ C_type.j_to_string ty ^ ") = ?");
      Ok Unknown)
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

  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _}
    when Variable.name n = "divUp" ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    (*  (n1 + n2 - 1)/n2 *)
    let n2_minus_1 : i_nexp = Bin (Minus, n2, NExp (Num 1)) in
    let n1_plus_n2_minus_1 : i_nexp = Bin (Plus, n1, NExp n2_minus_1) in
    ret_n (Bin (Div, NExp n1_plus_n2_minus_1, n2))

  | CallExpr {func = Ident {name=f; kind=Function; _}; args = [n]; _} when Variable.name f = "__is_pow2" ->
    let* n = parse_e "arg" n in
    ret_b (Pred ("pow2", n))
  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _} when Variable.name n = "min" ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    ret_n (NIf (BExp (NRel (NLt, n1, n2)), n1, n2))
  | CallExpr {func = Ident {name=n; kind=Function; _}; args = [n1; n2]; _} when Variable.name n = "max" ->
    let* n1 = parse_e "lhs" n1 in
    let* n2 = parse_e "rhs" n2 in
    ret_n (NIf (BExp (NRel (NGt, n1, n2)), n1, n2))
  | BinaryOperator {lhs=l; opcode="&"; rhs=IntegerLiteral 1; _} ->
    let* n = parse_exp l in
    ret_b (NRel (NEq, NExp (Bin (Mod, n, NExp (Num 2))), NExp (Num 0)))

  | BinaryOperator {opcode="&"; lhs=n1; rhs=BinaryOperator {opcode="-"; lhs=n2; rhs=IntegerLiteral 1; _}; ty=ty} ->
    parse_exp (BinaryOperator {opcode="%"; lhs=n1; rhs=n2; ty=ty})
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
  | UnaryOperator {opcode="~"; _}
  | CXXConstructExpr _
  | MemberExpr _
  | CallExpr _ 
  | UnaryOperator _
  | CXXOperatorCallExpr _ ->
    L.warning ("WARNING: parse_exp: rewriting to unknown: " ^ D_lang.Expr.to_string e);
    Ok Unknown

  | _ ->
    root_cause ("WARNING: parse_nexp: unsupported expression " ^ D_lang.Expr.name e ^ " : " ^ D_lang.Expr.to_string e)

let parse_type (e:D_lang.d_type) : C_type.t d_result =
  e
  |> C_type.from_json
  |> Result.map_error (fun e ->
    Common.StackTrace.RootCause (Rjson.error_to_string e)
  )

module Arg = struct
  type i_array = {address: Variable.t; offset: i_exp}

  type t =
  | Scalar of i_exp
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
      Ok {l with offset = NExp (Bin (Plus, offset, l.offset))}
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
(* -------------------------------------------------------------- *)


(* 
   Parsing expressions requires a global state
   which is a set of unknowns. Whenever we parse a
   statement we may create certain unknowns. Such variables
   are defined within the scope of that statement alone.
   The state UnknownSt is global to parsing numerical
   and boolean expressions.
 *)
module Unknown = struct
  type t = int

  let make : t = 0

  let get_var (st:t) = Variable.from_name ("__unk" ^ string_of_int st)

  let create (st:t) : t * Variable.t =
    (st + 1, get_var st)

  let get (st:t) : Variable.Set.t =
    let rec add (c:int) : Variable.Set.t =
      if c <= 0 then Variable.Set.empty
      else
        let c = c - 1 in
        Variable.Set.add (get_var c) (add c)
    in
    add st

  let rec handle_n (u:t) (e:i_exp) : (t * nexp) =
    match e with
    | NExp n ->
      (match n with
      | Var x -> (u, Exp.Var x)
      | Num x -> (u, Exp.Num x)
      | Bin (o, n1, n2) ->
        let (u, n1) = handle_n u n1 in
        let (u, n2) = handle_n u n2 in
        (u, Exp.Bin (o, n1, n2))
      | NCall (x, n) ->
        let (u, n) = handle_n u n in
        (u, Exp.NCall (x, n)) 
      | NIf (b, n1, n2) ->
        let (u, b) = handle_b u b in
        let (u, n1) = handle_n u n1 in
        let (u, n2) = handle_n u n2 in
        (u, Exp.NIf (b, n1, n2)))
    | BExp _ ->
      let (u, b) = handle_b u e in
      (u, Exp.NIf (b, Num 1, Num 0))
    | Unknown ->
      let (u, x) = create u in
      (u, Exp.Var x)

  and handle_b (u:t) (e:i_exp) : (t * bexp) =
    match e with
    | BExp b ->
      (match b with
      | Bool x -> (u, Exp.Bool x)
      | NRel (o, n1, n2) ->
        let (u, n1) = handle_n u n1 in
        let (u, n2) = handle_n u n2 in
        (u, NRel (o, n1, n2))
      | BRel (o, b1, b2) ->
        let (u, b1) = handle_b u b1 in
        let (u, b2) = handle_b u b2 in
        (u, BRel (o, b1, b2))
      | BNot b ->
        let (u, b) = handle_b u b in
        (u, BNot b)
      | ThreadEqual n ->
        let (u, n) = handle_n u n in
        (u, ThreadEqual n)
      | Pred (x, n) ->
        let (u, n) = handle_n u n in
        (u, Pred (x, n)))
    | NExp _ ->
      let (u, n) = handle_n u e in
      (u, NRel (NNeq, n, Num 0))
    | Unknown ->
      let (u, x) = create u in
      (u, NRel (NNeq, Var x, Num 0))

  let convert (handler:t -> 'a -> t * 'b) (n:'a) : Variable.Set.t * 'b =
    let (u, n) = handler make n in
    (get u, n)

  (* Convert a d_nexp into an nexp and get the set of unknowns *)
  let to_nexp: i_exp -> Variable.Set.t * nexp = convert handle_n

  let handle_arg (u:t) : Arg.t -> (t * Imp.Arg.t) =
    function
    | Scalar e ->
      let (u, e) = handle_n u e in
      (u, Scalar e)
    | Array {offset; address} ->
      let (u, offset) = handle_n u offset in
      (u, Array {address; offset})
    | Unsupported ->
      (u, Unsupported)

  let to_arg : Arg.t -> Variable.Set.t * Imp.Arg.t =
    convert handle_arg

  (* Convert a d_bexp into an bexp and get the set of unknowns *)
  let to_bexp: i_exp -> Variable.Set.t * bexp = convert handle_b

  let rec mmap (f:t -> 'a -> t * 'b) (st:t) : 'a list -> (t * 'b list) =
    function
    | [] -> (st, [])
    | x :: l ->
      let (st, x) = f st x in
      let (st, l) = mmap f st l in
      (st, x :: l) 

  let to_nexp_list: i_exp list -> Variable.Set.t * nexp list =
    convert (mmap handle_n)

  let to_arg_list : Arg.t list -> Variable.Set.t * Imp.Arg.t list =
    convert (mmap handle_arg)

  (* Convert a d_nexp into an nexp only if there are no unknowns *)
  let try_to_nexp (n:i_exp) : nexp option =
    let (u, n) = handle_n make n in
    if u = 0
    then Some n
    else None

  (* Convert a d_bexp into an bexp only if there are no unknowns *)
  let try_to_bexp (n:i_exp) : bexp option =
    let (u, b) = handle_b make n in
    if u = 0
    then Some b
    else None

  let as_decls (xs:Variable.Set.t) : (Variable.t * nexp option) list =
    Variable.Set.elements xs |> List.map (fun x -> (x, None))

  let decl_unknown (vars:Variable.Set.t) : Imp.Stmt.t list =
    if Variable.Set.is_empty vars then []
    else
      [Decl (as_decls vars)]

  let ret_u (vars:Variable.Set.t) (s:Imp.Stmt.t) : Imp.Stmt.t list d_result =
    Ok (decl_unknown vars @ [s])

  let ret_f ?(extra_vars=Variable.Set.empty) (f:'a -> Variable.Set.t * 'b) (handler:'b -> Imp.Stmt.t) (n:'a) : Imp.Stmt.t list d_result =
    let vars, n = f n in
    let vars = Variable.Set.union extra_vars vars in
    ret_u vars (handler n)

  let ret_n ?(extra_vars=Variable.Set.empty): (nexp -> Imp.Stmt.t) -> i_exp -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars to_nexp

  let ret_ns ?(extra_vars=Variable.Set.empty): (nexp list -> Imp.Stmt.t) -> i_exp list -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars to_nexp_list

  let ret_args ?(extra_vars=Variable.Set.empty): (Imp.Arg.t list -> Imp.Stmt.t) -> Arg.t list -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars to_arg_list

  let ret_b ?(extra_vars=Variable.Set.empty): (bexp -> Imp.Stmt.t) -> i_exp -> Imp.Stmt.t list d_result =
    ret_f ~extra_vars to_bexp


end


(* -------------------------------------------------------------- *)

let cast_map f = Rjson.map_all f (fun idx _ e ->
  StackTrace.Because ("Error parsing list: error in index #" ^ (string_of_int (idx + 1)), e))

let parse_decl (d:D_lang.Decl.t) : (Variable.t * nexp option) list d_result =
  let parse_e m b = with_msg (m ^ ": " ^ D_lang.Decl.to_string d) parse_exp b in
  let* ty = match C_type.from_json d.ty_var.ty with
  | Ok ty -> Ok ty
  | Error _ -> root_cause ("parse_decl: error parsing type: " ^ Rjson.pp_js d.ty_var.ty)
  in
  if C_type.is_int ty
  then (
    let* ((vars, n):(Variable.Set.t * (nexp option))) = match d.init with
    | Some (IExpr n) ->
      let* n = parse_e "init" n in
      let (vars, n) = Unknown.to_nexp n in
      Ok (vars, Some n)
    | _ -> Ok (Variable.Set.empty, None)
    in
    Ok ((d.ty_var.name, n) :: Unknown.as_decls vars )
  ) else (
    L.warning ("parse_decl: skipping non-int local variable '" ^ Variable.name d.ty_var.name ^ "' type: " ^ Rjson.pp_js d.ty_var.ty);
    Ok []
  )

let is_pointer (j:Yojson.Basic.t) =
  match C_type.from_json j with
  | Ok t -> C_type.is_pointer t
  | Error _ -> false

let rec parse_load_expr (target:D_lang.Expr.t) (exp:D_lang.Expr.t)
  : (d_location_alias, D_lang.Expr.t) Either.t =
  let open Either in
  match exp with
  | Ident {ty=ty; _} when is_pointer ty ->
    Left {target=target; source=exp; offset=IntegerLiteral 0}
  | BinaryOperator ({lhs=l; _} as b) ->
    (match parse_load_expr target l with
    | Left l -> Left {l with offset =BinaryOperator {b with lhs=l.offset}}
    | Right _ -> Right exp)
  | _ ->
    Right exp



let parse_location_alias (s:d_location_alias) : Imp.Stmt.t list d_result =
  let* source = with_msg "location_alias.source" parse_var s.source in
  let* target = with_msg "location_alias.target" parse_var s.target in
  let* offset = with_msg "location_alias.offset" parse_exp s.offset in
  offset |> Unknown.ret_n (fun offset ->
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
      (lb, Bin (Minus, ub, Num 1), Range.Increase)
    (* (int i = 4; i >= 0; i--) *)
    | {init=ub; cond={op=GtEq; arg=lb; _}; _} ->
      (lb, ub, Decrease)
    (* (int i = 0; i <= 4; i++) *)
    | {init=lb; cond={op=LtEq; arg=ub; _}; _} ->
      (lb, ub, Increase)
    (* (int i = 4; i > 0; i--) *)
    | {init=ub; cond={op=Gt; arg=lb; _}; _} ->
      (Bin (Plus, Num 1, lb), ub, Decrease)

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
    }

  let parse_unop (u:'a Loop_infer.unop) : 'a unop option d_result =
    let* arg = parse_exp u.arg in
    Ok (match Unknown.try_to_nexp arg with
      | Some arg -> Some {op=u.op; arg=arg}
      | None -> None)

  let from_loop_infer (r:Loop_infer.t) : t option d_result =
    let* init = parse_exp r.init in
    let* cond = parse_unop r.cond in
    let* inc = parse_unop r.inc in
    Ok (match Unknown.try_to_nexp init, cond, inc with
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

let ret_assert (b:D_lang.Expr.t) : Imp.Stmt.t list d_result =
  let* b = with_msg "cond" parse_exp b in
  match Unknown.try_to_bexp b with
  | Some b -> ret (Assert b)
  | None -> ret_skip

let rec parse_stmt (sigs:Variable.t list Variable.Map.t) (c:D_lang.Stmt.t) : Imp.Stmt.t list d_result =
  let parse_stmt = parse_stmt sigs in
  let with_msg (m:string) f b = with_msg_ex (fun _ -> "parse_stmt: " ^ m ^ ": " ^ D_lang.Stmt.summarize c) f b in
  let ret_n = Unknown.ret_n in
  let ret_b = Unknown.ret_b in
  let ret_ns = Unknown.ret_ns in
  let ret_args = Unknown.ret_args in

  match c with

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args=[]; _})
    when Variable.name n = "__syncthreads" ->
    ret (Sync n.location)

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args=[_]; _})
    when Variable.name n = "sync" ->
    ret (Sync n.location)

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args = [b]; _})
    when Variable.name n = "__requires" ->
    ret_assert b

  | SExpr (CallExpr {func = Ident {name=n; kind=Function; _}; args;_ }) ->
    (match Variable.Map.find_opt n sigs with
    | Some params ->
      if List.length params = List.length args then (
        let* args = with_msg "call.args" (cast_map Arg.parse) args in
        args |> ret_args (fun args ->
          let args = List.map2 (fun x y -> (x, y)) params args in
          Imp.Stmt.Call {kernel=n; args}
        )
      ) else
        root_cause "Args mismatch!"
    | None ->
      ret (Block [])
    )

  | WriteAccessStmt w ->
    let x = w.target.name |> Variable.set_location w.target.location in
    let* idx = with_msg "write.idx" (cast_map parse_exp) w.target.index in
    idx |> ret_ns (fun idx ->
      Write {array=x; index=idx; payload=w.payload}
    )

  | ReadAccessStmt r ->
    let x = r.source.name |> Variable.set_location r.source.location in
    let* idx = with_msg "read.idx" (cast_map parse_exp) r.source.index in
    idx
    |> ret_ns (fun idx ->
      Read {target=r.target; array=x; index=idx}
    )

  | IfStmt {cond=b;then_stmt=CompoundStmt[ReturnStmt];else_stmt=CompoundStmt[]} 
  | IfStmt {cond=b;then_stmt=ReturnStmt;else_stmt=CompoundStmt[]} ->
    ret_assert (UnaryOperator {opcode="!"; child=b; ty=D_lang.Expr.to_type b})

  | IfStmt c ->
    let* b = with_msg "if.cond" parse_exp c.cond in
    let* t = with_msg "if.then" parse_stmt c.then_stmt in
    let* e = with_msg "if.else" parse_stmt c.else_stmt in
    b |> ret_b (fun b -> Imp.Stmt.s_if b (Block t) (Block e))

  | CompoundStmt l ->
    let* l = with_msg "block" (cast_map parse_stmt) l in
    ret (Block (List.flatten l))

  (* Support for location aliasing that declares a new variable *)
  | DeclStmt ([{ty_var={ty=ty; _} as lhs; init=Some (IExpr rhs); _}] as l)
    when is_pointer ty
    ->
    let lhs : D_lang.Expr.t = Ident (Decl_expr.from_ty_var lhs) in
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

  | SExpr ((BinaryOperator {opcode="="; lhs=Ident {ty=ty; _} as lhs; rhs=rhs; _}))
    when is_pointer ty
    ->
    (match parse_load_expr lhs rhs with
    | Left a ->
      parse_location_alias a
    | Right _ -> Ok [])

  | SExpr (BinaryOperator {opcode="="; lhs=Ident {name=v; _}; rhs=rhs; _})
    ->
    let* rhs = with_msg "assign.rhs" parse_exp rhs in
    rhs |> ret_n (fun rhs -> Decl [v, Some rhs])

  | ContinueStmt
  | BreakStmt
  | GotoStmt
  | ReturnStmt 
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

type param = (Variable.t, Variable.t * Memory.t) Either.t

let from_j_error (e:Rjson.j_error) : d_error =
  RootCause (Rjson.error_to_string e)

let parse_param (p:Param.t) : param option d_result =
  let mk_array (h:Memory.Hierarchy.t) (ty:C_type.t) : Memory.t =
    {
      hierarchy = h;
      size = C_type.get_array_length ty;
      data_type = C_type.get_array_type ty;
    }
  in
  let* ty = C_type.from_json p.ty_var.ty |> Result.map_error from_j_error in
  if C_type.is_int ty then
    Ok (Some (Either.Left p.ty_var.name))
  else if C_type.is_array ty then (
    let h = if p.is_shared then Memory.Hierarchy.SharedMemory else Memory.Hierarchy.GlobalMemory in
    Ok (Some (Either.Right (p.ty_var.name, mk_array h ty)))
  ) else Ok None

let parse_params (ps:Param.t list) : (Variable.Set.t * Memory.t Variable.Map.t) d_result =
  let* params = Rjson.map_all parse_param
    (fun i _ e -> StackTrace.Because ("Error in index #" ^ string_of_int i, e)) ps in
  let globals, arrays = Common.flatten_opt params |> Common.either_split in
  Ok (Variable.Set.of_list globals, Variable.MapUtil.from_list arrays)

let cuda_preamble (tail:Imp.Stmt.t) : Variable.Set.t * Imp.Stmt.t =
  let open Exp in
  let mk_dims (name:string) : (Variable.t * nexp option) list =
    List.map (fun x -> (Variable.from_name (name ^ "." ^ x),  None) ) cuda_dims
  in
  let local_dims = List.concat_map mk_dims (List.map fst cuda_local_vars) in
  let mk_var (name:string) (suffix:string) (x:string) : nexp =
    Var (Variable.from_name (name ^ suffix ^ "." ^ x))
  in
  let all_vars_constraints : bexp list =
    cuda_dims
    |> List.concat_map (fun (x:string) ->
      all_cuda_vars
      |> List.map (fun (name, lower_bound) ->
        n_ge (Var (Variable.from_name (name ^ "." ^ x))) (Num lower_bound)
      )
    )
  in
  let idx_lt_dim (name1, name2) : bexp list =
    List.map (fun x ->
      n_lt (mk_var name1 "Idx" x) (mk_var name2 "Dim" x)
    ) cuda_dims
  in
  let var_list (vars : (string*int) list) : Variable.t list =
    cuda_dims
    |> List.concat_map (fun (x:string) ->
      vars
      |> List.map fst
      |> List.map (fun (name:string) ->
          Variable.from_name (name ^ "." ^ x)
      )
    )
  in
  let local_vars : Variable.t list = var_list cuda_local_vars in
  let global_vars : Variable.Set.t =
    cuda_global_vars
    |> var_list
    |> Variable.Set.of_list
  in
  let open Imp in
  global_vars,
  Block [
    Decl local_dims;
    Assert (
      all_vars_constraints
      @
      (Exp.distinct local_vars ::
        List.concat_map idx_lt_dim [("thread", "block"); ("block", "grid")]
      )
      |> b_and_ex
    );
    tail
  ]

let mk_array (h:Memory.Hierarchy.t) (ty:C_type.t) : Memory.t =
  {
    hierarchy = h;
    size = C_type.get_array_length ty;
    data_type = C_type.get_array_type ty;
  }

let parse_shared (s:D_lang.Stmt.t) : (Variable.t * Memory.t) list =
  let open D_lang in
  let rec find_shared
    (arrays:(Variable.t * array_t) list)
    (s:Stmt.t)
  : (Variable.t * array_t) list =
    match s with
    | DeclStmt l ->
      List.filter_map (fun (d:Decl.t) ->
        Decl.get_shared d
        |> Option.map (fun a -> (d.ty_var.name, a))
      ) l
      |> Common.append_tr arrays
    | WriteAccessStmt _
    | ReadAccessStmt _
    | GotoStmt
    | ReturnStmt
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
  (sigs:Variable.t list Variable.Map.t)
  (shared_params:(Variable.t * Memory.t) list)
  (globals:Variable.t list)
  (assigns:(Variable.t * nexp) list)
  (k:D_lang.Kernel.t)
: Imp.Kernel.t d_result =
  let* code = parse_stmt sigs k.code in
  let* (params, arrays) = parse_params k.params in
  let params =
    globals
    |> Variable.Set.of_list
    |> Variable.Set.union params
  in 
  let shared = parse_shared k.code
    |> Common.append_rev1 shared_params
    |> Variable.MapUtil.from_list in
  let rec add_type_params (params:Variable.Set.t) : Ty_param.t list -> Variable.Set.t =
    function
    | [] -> params
    | TemplateType _ :: l -> add_type_params params l
    | NonTypeTemplate x :: l ->
      let params = match C_type.from_json x.ty with
      | Ok ty when C_type.is_int ty -> Variable.Set.add x.name params
      | _ -> params
      in
      add_type_params params l
  in
  let open Imp.Stmt in
  let globals, code =
    let assigns = Decl (List.map (fun (k,v) -> (k, Some v)) assigns) in
    cuda_preamble (Block (assigns :: code))
  in
  let params =
    add_type_params params k.type_params
    |> Variable.Set.union globals
  in
  let open Imp.Kernel in
  Ok {
    name = k.name;
    pre = Exp.b_true; (* TODO: implement this *)
    code = code;
    params = params;
    arrays = Variable.Map.union (fun _ _ r -> Some r) arrays shared;
    visibility =
      match k.attribute with
      | Default -> Global
      | Auxiliary -> Device
    ;
  }

let parse_program (p:D_lang.Program.t) : Imp.Kernel.t list d_result =
  let sigs = D_lang.Program.kernel_signatures p in
  let rec parse_p
    (arrays:(Variable.t * Memory.t) list)
    (globals:Variable.t list)
    (assigns:(Variable.t * nexp) list)
    (p:D_lang.Program.t)
  : Imp.Kernel.t list d_result =
    match p with
    | Declaration v :: l ->
      let (arrays, globals, assigns) =
          match C_type.from_json v.ty_var.ty with
          | Ok ty ->
            if List.mem C_lang.c_attr_shared v.attrs then
              (v.ty_var.name, mk_array SharedMemory ty)::arrays, globals, assigns
            else if C_type.is_int ty then
              let g = match v.init with
              | Some (IExpr n) ->
                (match parse_exp n with
                | Ok n -> Unknown.try_to_nexp n
                | Error _ -> None) 
              | _ -> None
              in
              match g with
              | Some g -> arrays, globals, (v.ty_var.name, g) :: assigns
              | None -> arrays, v.ty_var.name :: globals, assigns
            else
              arrays, globals, assigns
          | Error _ -> arrays, globals, assigns
      in
      parse_p arrays globals assigns l
    | Kernel k :: l ->
      let* ks = parse_p arrays globals assigns l in
      let* k = parse_kernel sigs arrays globals assigns k in
      Ok (k::ks)
    | [] -> Ok []
  in
  parse_p [] [] [] p
end

module Default = Make(Logger.Colors)
module Silent = Make(Logger.Silent)
