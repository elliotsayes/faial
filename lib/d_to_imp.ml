module StackTrace = Common.StackTrace

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

let parse_nbin: string -> nbin option =
  function
  | "+" -> Some Plus
  | "-" -> Some Minus
  | "*"  -> Some Mult
  | "/" -> Some Div
  | "%" -> Some Mod
  | ">>" -> Some RightShift
  | "<<" -> Some LeftShift
  | "^" -> Some BitXOr
  | "|" -> Some BitOr
  | "&" -> Some BitAnd
  | x ->
    prerr_endline ("WARNING: parse_nbin: Can't handle '(int, int) int' operator '"^ x ^"' converting it to unknown");
    None

let parse_nrel_opt: string -> nrel option =
  function
  | "==" -> Some NEq
  | "!=" -> Some NNeq
  | "<=" -> Some NLe
  | "<"  -> Some NLt
  | ">=" -> Some NGe
  | ">"  -> Some NGt
  | x -> None

let parse_brel: string -> bexp -> bexp -> bexp =
  function
  | "||" -> b_or
  | "&&" -> b_and
  | x ->
    prerr_endline ("WARNING: parse_brel: Can't handle " ^ x ^ " converting it to unknown");
    fun b1 b2 -> BUnknown

let with_msg (msg:string) (f:'a -> 'b d_result) (c:'a): 'b d_result =
  match f c with
  | Ok o -> Ok o
  | Error err -> Error (Because (msg, err))

let with_exp (msg:string) (e: Dlang.d_exp) : (Dlang.d_exp -> 'a d_result) -> Dlang.d_exp -> 'a d_result =
  with_msg (msg ^ ": " ^ Dlang.exp_to_s e)

let parse_var: Dlang.d_exp -> variable d_result =
  function
  | NonTypeTemplateParmDecl { name = v ; _ }
  | ParmVarDecl { name = v ; _ }
  | VarDecl { name = v ; _ }
  | FunctionDecl { name = v; _ } -> Ok v
  | e -> root_cause ("parse_var: unexpected expression: " ^ Dlang.exp_to_s e)

let is_variable : Dlang.d_exp -> bool =
  function
  | NonTypeTemplateParmDecl _
  | ParmVarDecl _
  | VarDecl _
    -> true
  | _ -> false

type d_access = {location: variable; mode: mode; index: Dlang.d_exp list }

type d_location_alias = {
  source: Dlang.d_exp;
  target: Dlang.d_exp;
  offset: Dlang.d_exp
}

let cuda_global_vars = [
  "threadDim";
  "blockIdx"; "blockDim";
  "gridIdx"; "gridDim";
]

let cuda_local_vars = ["threadIdx"]

let cuda_vars = cuda_local_vars @ cuda_global_vars

let cuda_dims = ["x"; "y"; "z"]

let rec parse_nexp (e: Dlang.d_exp) : nexp d_result =
  let parse_b m b = with_exp m e parse_bexp b in
  let parse_n m n = with_exp m e parse_nexp n in
  match e with
  (* ---------------- CUDA SPECIFIC ----------- *)
  | MemberExpr {base=VarDecl{name=base}; name=dim} 
    when List.mem (var_name base) cuda_vars && List.mem dim cuda_dims->
      let x = var_name base ^ "." ^ dim |> var_make in
      Ok (Var x)

  (* ------------------------------------------ *)
  | NonTypeTemplateParmDecl { name = v ; _ }
  | ParmVarDecl { name = v ; _ }
  | VarDecl { name = v ; _ }
    -> Ok (Var v)
  | IntegerLiteral n
  | CharacterLiteral n -> Ok (Num n)
  | FloatingLiteral n -> 
    prerr_endline ("WARNING: parse_nexp: converting float '" ^ Float.to_string n ^ "' to integer");
    Ok (Num (Float.to_int n))
  | ConditionalOperator o ->
    let* b = parse_b "cond" o.cond in
    let* n1 = parse_n "then_expr" o.then_expr in
    let* n2 = parse_n "else_expr" o.else_expr in
    Ok (n_if b n1 n2)
  | CallExpr {func = FunctionDecl {name = n; _}; args = [n1; n2]} when var_name n = "min" ->
    let* n1 = parse_n "lhs" n1 in
    let* n2 = parse_n "rhs" n2 in
    Ok (n_if (n_lt n1 n2) n1 n2)
  | CallExpr {func = FunctionDecl {name = n; _}; args = [n1; n2]} when var_name n = "max" ->
    let* n1 = parse_n "lhs" n1 in
    let* n2 = parse_n "rhs" n2 in
    Ok (n_if (n_gt n1 n2) n1 n2)
  | BinaryOperator {opcode=o; lhs=n1; rhs=n2} ->
    let* n1 = parse_n "lhs" n1 in
    let* n2 = parse_n "rhs" n2 in
    Ok (match parse_nbin o with
    | Some o -> n_bin o n1 n2
    | None -> NUnknown)
  | CXXBoolLiteralExpr b ->
    Ok (Num (if b then 1 else 0))
  | UnaryOperator {opcode="~"; _}
  | CXXConstructExpr _
  | MemberExpr _
  | FunctionDecl _ 
  | EnumConstantDecl _
  | CallExpr _ ->
    prerr_endline ("WARNING: parse_nexp: rewriting to unknown: " ^ Dlang.exp_to_s e);
    Ok NUnknown

  | _ ->
    root_cause ("WARNING: parse_nexp: unsupported expression " ^ Dlang.exp_name e ^ " : " ^ Dlang.exp_to_s e)

and parse_bexp (e: Dlang.d_exp) : bexp d_result =
  let parse_b m b = with_exp ("parse_bexp: " ^ m) e parse_bexp b in
  let parse_n m n = with_exp ("parse_bexp: " ^ m) e parse_nexp n in
  match e with
  | BinaryOperator {lhs=l; opcode="&"; rhs=IntegerLiteral 1} ->
    let* n = parse_nexp l in
    Ok (n_eq (n_mod n (Num 2)) (Num 0))
  | BinaryOperator o ->
    (match parse_nrel_opt o.opcode with
    | Some r ->
      let* n1 = parse_n "lhs" o.lhs in
      let* n2 = parse_n "rhs" o.rhs in
      Ok (n_rel r n1 n2)
    | None ->
      let* b1 = parse_b "lhs" o.lhs in
      let* b2 = parse_b "rhs" o.rhs in
      Ok (parse_brel o.opcode b1 b2)
    )

  | UnaryOperator u when u.opcode = "!" ->
    let* b = parse_b "not" u.child in
    Ok (b_not b)

  | _ ->
    let* n = parse_n (Dlang.exp_name e) e in
    prerr_endline ("WARNING: parse_bexp: rewriting '" ^ Dlang.exp_to_s e ^ "' to: 0 != " ^ Dlang.exp_to_s e);
    Ok (n_neq n (Num 0))


let cast_map f = Rjson.map_all f (fun idx s e ->
  StackTrace.Because ("Error in index #" ^ (string_of_int (idx + 1)), e))

let parse_access (a:d_access) : Exp.acc_expr d_result =
  let* i = with_msg "parse_access: index" (cast_map parse_nexp) a.index in
  Ok (a.location, {access_index=i; access_mode=a.mode})


let parse_decl (d:Dlang.d_decl) : (variable * Imp.locality * nexp option) option d_result =
  let parse_n m b = with_msg (m ^ ": " ^ Dlang.decl_to_s d) parse_nexp b in
  let* ty = match Cast.parse_type d.ty with
  | Ok ty -> Ok ty
  | Error _ -> root_cause ("parse_decl: error parsing type: " ^ Rjson.pp_js d.ty)
  in
  if Ctype.is_int ty
  then (
    let* (n:Exp.nexp option) = match d.init with
    | Some (IExp n) ->
      let* n = parse_n "init" n in
      Ok (Some n)
    | _ -> Ok None
    in
    Ok (Some (d.name, Imp.Local, n))
  ) else (
    prerr_endline ("WARNING: parse_decl: skipping non-int local variable '" ^ var_name d.name ^ "' type: " ^ Rjson.pp_js d.ty);
    Ok None
  )

let is_pointer (j:Yojson.Basic.t) =
  match Cast.parse_type j with
  | Ok t -> Ctype.is_pointer t
  | Error _ -> false

let rec parse_load_expr (target:Dlang.d_exp) (exp:Dlang.d_exp)
  : (d_location_alias, Dlang.d_exp) Either.t =
  let open Imp in
  let open Either in
  match exp with
  | VarDecl {ty=ty; _}
  | ParmVarDecl {ty=ty; _} when is_pointer ty ->
    Left {source=exp; target=target; offset=IntegerLiteral 0}
  | BinaryOperator ({lhs=l; rhs=r; _} as b) ->
    (match parse_load_expr target l with
    | Left l -> Left {l with offset =BinaryOperator {b with lhs=l.offset}}
    | Right _ -> Right exp)
  | _ ->
    Right exp

let parse_location_alias (s:d_location_alias) : Imp.stmt d_result =
  let* source = with_msg "location_alias.source" parse_var s.source in
  let* target = with_msg "location_alias.target" parse_var s.target in
  let* offset = with_msg "location_alias.offset" parse_nexp s.offset in
  let open Imp in
  Ok (LocationAlias {
    alias_source=source;
    alias_target=target;
    alias_offset=offset
  })


type 'a unop =
  {op: 'a; arg: nexp}

type for_range = {
  name: variable;
  init: nexp;
  cond: Loops.comparator unop;
  inc: Loops.increment unop;
}

let parse_unop (u:'a Loops.unop) : 'a unop d_result =
  let* arg = parse_nexp u.arg in
  Ok {op=u.op; arg=arg}

let infer_range (r:Dlang.d_for) : Exp.range option d_result =
  let parse_for_range (r:Loops.d_for_range) : for_range d_result =
    let* init = parse_nexp r.init in
    let* cond = parse_unop r.cond in
    let* inc = parse_unop r.inc in
    Ok {name = r.name; init=init; cond=cond; inc=inc}
  in
  let infer_range (r:for_range) : Exp.range option =
    let open Loops in
    let open Exp in
    let (let*) = Option.bind in
    let (lb, ub) = match r with
    (* (int i = 0; i < 4; i++) *)
    | {init=lb; cond={op=Lt; arg=ub}} ->
      (lb,ub)
    (* (int i = 4; i >= 0; i--) *)
    | {init=ub; cond={op=GtEq; arg=lb}}
    (* (int i = 0; i <= 4; i++) *)
    | {init=lb; cond={op=LtEq; arg=ub}} ->
      (lb, n_plus (Num 1) ub)
    (* (int i = 4; i > 0; i--) *)
    | {init=ub; cond={op=Gt; arg=lb}} ->
      (n_plus (Num 1) lb, n_plus (Num 1) ub)
    in
    let* step = match r.inc with
    | {op=Plus; arg=a}
    | {op=Minus; arg=a} -> Some (Default a)
    | {op=Mult; arg=Num a}
    | {op=Div; arg=Num a} ->
      Some (StepName (Printf.sprintf "pow%d" a))
    | {op=LShift; arg=Num a}
    | {op=RShift; arg=Num a} ->
      Some (StepName (Printf.sprintf "pow%d" (Predicates.pow 2 a)))
    | _ -> None
    in
    Some {
      range_var=r.name;
      range_lower_bound=lb;
      range_upper_bound=ub;
      range_step=step;
    }
  in
  match Loops.parse_for r with
  | Some r ->
    let* r = parse_for_range r in
    Ok (infer_range r)
  | None -> Ok None


let rec parse_stmt (c:Dlang.d_stmt) : Imp.stmt d_result =
  let with_msg (m:string) f b = with_msg ("parse_stmt: " ^ m) f b in

  match c with

  | SExp (CallExpr {func=FunctionDecl{name=n}; args=[]})
    when var_name n = "__syncthreads" ->
    Ok Imp.Sync

  | SExp (CallExpr {func = FunctionDecl {name = n; _}; args = [b]})
    when var_name n = "__requires" ->
    let* b = with_msg "cond" parse_bexp b in
    Ok (Imp.Assert b)


  | WriteAccessStmt w ->
    let x = w.target.name in
    let* idx = with_msg "write.idx" (cast_map parse_nexp) w.target.index in
    Ok (Imp.Acc (x, {access_index=idx; access_mode=W}))

  | ReadAccessStmt r ->
    let x = r.source.name in
    let* idx = with_msg "read.idx" (cast_map parse_nexp) r.source.index in
    Ok (Imp.Acc (x, {access_index=idx; access_mode=R}))

  | IfStmt {cond=b;then_stmt=CompoundStmt[ReturnStmt];else_stmt=CompoundStmt[]} 
  | IfStmt {cond=b;then_stmt=ReturnStmt;else_stmt=CompoundStmt[]} ->
    let* b = with_msg "cond" parse_bexp b in
    let open Imp in
    Ok (Imp.Assert (b_not b))

  | IfStmt c ->
    let* b = with_msg "if.cond" parse_bexp c.cond in
    let* t = with_msg "if.then" parse_stmt c.then_stmt in
    let* e = with_msg "if.else" parse_stmt c.else_stmt in
    let open Imp in
    Ok (s_if b t e)

  | CompoundStmt l ->
    let* l = cast_map parse_stmt l in
    Ok (Imp.Block l)

  | DeclStmt l ->
    let* l = cast_map parse_decl l |> Result.map Common.flatten_opt in
    Ok (Imp.Decl l)

  | SExp ((BinaryOperator {opcode="="; lhs=VarDecl {name=v; ty=ty} as lhs; rhs=rhs}))
  | SExp ((BinaryOperator {opcode="="; lhs=ParmVarDecl {name=v; ty=ty} as lhs; rhs=rhs}))
    when is_pointer ty
    ->
    (match parse_load_expr lhs rhs with
    | Left a ->
      parse_location_alias a
    | Right _ -> Ok (Imp.Block []))

  | SExp (BinaryOperator {opcode="="; lhs=VarDecl {name=v; ty=ty}; rhs=rhs})
  | SExp (BinaryOperator {opcode="="; lhs=ParmVarDecl {name=v; ty=ty}; rhs=rhs})
    ->
    let* rhs = with_msg "assign.rhs" parse_nexp rhs in
    let open Imp in 
    Ok (Decl [v, Local, Some rhs])

  | BreakStmt
  | GotoStmt
  | ReturnStmt 
  | SExp _ -> Ok (Imp.Block [])

  | ForStmt s ->
    let* r = infer_range s in
    let* b = with_msg "for.body" parse_stmt s.body in
    let open Imp in
    (match r with
    | Some r -> Ok (For (r, b))
    | None -> Ok (Loop b))

  | DoStmt {cond=cond; body=body} ->
    let* body = with_msg "do.body" parse_stmt body in
    let open Imp in
    Ok (Loop body)

  | WhileStmt {cond=cond; body=body} ->
    let* body = with_msg "while.body" parse_stmt body in
    let open Imp in
    Ok (Loop body)

  | SwitchStmt s ->
    with_msg "switch.body" parse_stmt s.body

  | CaseStmt s ->
    with_msg "case.body" parse_stmt s.body

  | DefaultStmt s ->
    with_msg "default.body" parse_stmt s

type param = (variable, variable * array_t) Either.t

let from_j_error (e:Rjson.j_error) : d_error =
  RootCause (Rjson.error_to_string e)

let parse_param (p:Cast.c_param) : param option d_result =
  let mk_array (h:hierarchy_t) (ty:Ctype.t) : array_t =
    {
      array_hierarchy = h;
      array_size = Ctype.get_array_length ty;
      array_type = Ctype.get_array_type ty;
    }
  in
  let* ty = Cast.parse_type p.ty |> Result.map_error from_j_error in
  if Ctype.is_int ty then
    Ok (Some (Either.Left p.name))
  else if Ctype.is_array ty then (
    let h = if p.is_shared then Exp.SharedMemory else Exp.GlobalMemory in
    Ok (Some (Either.Right (p.name, mk_array h ty)))
  ) else Ok None

let parse_params (ps:Cast.c_param list) : (VarSet.t * array_t VarMap.t) d_result =
  let* params = Rjson.map_all parse_param
    (fun i a e -> StackTrace.Because ("Error in index #" ^ string_of_int i, e)) ps in
  let globals, arrays = Common.flatten_opt params |> Common.either_split in
  Ok (VarSet.of_list globals, list_to_var_map arrays)

let cuda_preamble (tail:Imp.stmt) : Imp.stmt =
  let open Exp in
  let open Imp in
  let mk_dims h (name:string) : (variable * locality * nexp option) list =
    List.map (fun x -> (var_make (name ^ "." ^ x), h, None) ) cuda_dims
  in
  let all_vars =
    List.concat_map (mk_dims Global) cuda_global_vars
    @
    List.concat_map (mk_dims Local) cuda_local_vars
  in
  let mk_var (name:string) (suffix:string) (x:string) : nexp =
    Var (var_make (name ^ suffix ^ "." ^ x))
  in
  let idx_lt_dim (name1, name2) : bexp list =
    List.map (fun x ->
      n_lt (mk_var name1 "Idx" x) (mk_var name2 "Dim" x)
    ) cuda_dims
  in
  let local_vars : variable list =
    cuda_dims
    |> List.concat_map (fun (x:string) ->
      cuda_local_vars
      |> List.map (fun (name:string) ->
          var_make (name ^ "." ^ x)
      )
    )
  in 
  let open Imp in
  Block [
    Decl all_vars;
    Assert (
      (Exp.distinct local_vars ::
        List.concat_map idx_lt_dim [("thread", "block"); ("block", "grid")]
      )
      |> b_and_ex
    );
    tail
  ]

let mk_array (h:hierarchy_t) (ty:Ctype.t) : array_t =
  {
    array_hierarchy = h;
    array_size = Ctype.get_array_length ty;
    array_type = Ctype.get_array_type ty;
  }

let parse_shared (s:Dlang.d_stmt) : (variable * array_t) list =
  let open Dlang in
  match s with
  | CompoundStmt l ->
    l |> List.concat_map (function
      | DeclStmt l ->
        l |> Common.(map_opt (fun d ->
          if List.mem Cast.c_attr_shared d.attrs then
            (match Cast.parse_type d.ty with
            | Ok ty ->
              Some (d.name, mk_array SharedMemory ty)
            | Error _ -> None)
          else None
        ))
      | _ -> [])
  | _ -> []

let parse_kernel (shared_params:(variable * array_t) list) (k:Dlang.d_kernel) : Imp.p_kernel d_result =
  let* code = parse_stmt k.code in
  let* (params, arrays) = parse_params k.params in
  let shared = parse_shared k.code
    |> Common.append_rev shared_params
    |> Exp.list_to_var_map in
  let open Imp in
  Ok {
    p_kernel_name = k.name;
    p_kernel_pre = Exp.b_true; (* TODO: implement this *)
    p_kernel_code = cuda_preamble code;
    p_kernel_params = params;
    p_kernel_arrays = VarMap.union 
      (fun k l r -> Some r)
      arrays shared;
  }

let parse_program (p:Dlang.d_program) : Imp.p_kernel list d_result =
  let rec parse_p (params:(variable * array_t) list) (p:Dlang.d_program) : Imp.p_kernel list d_result =
    match p with
    | Declaration v :: l ->
      let params = match Cast.parse_type v.ty with
        | Ok ty -> (v.name, mk_array SharedMemory ty)::params
        | _ -> params
      in
      parse_p params l
    | Kernel k :: l ->
      let* k = parse_kernel params k in
      let* ks = parse_p params l in
      Ok (k::ks) 
    | [] -> Ok []
  in
  parse_p [] p
