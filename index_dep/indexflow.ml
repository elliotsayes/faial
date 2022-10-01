open Stage0
open Stage1
open Inference

open D_lang

module Index = struct
  type t =
    | Dependent
    | Independent

  let add (t1:t) (t2:t) : t =
    match t1, t2 with
    | Independent, Independent -> Independent
    | _, _ -> Dependent

  let sum : t list -> t = List.fold_left add Independent
  let to_string: t -> string =
    function
    | Dependent -> "dependent"
    | Independent -> "independent"
end

module Typing = struct
  type t = Index.t Variable.Map.t

  let make : t = Variable.Map.empty

  let add : t -> t -> t =
    Variable.Map.union (fun _ v1 v2 ->
      Some (Index.add v1 v2)
    )

  let get (x: Variable.t) (env:t) : Index.t =
    match Variable.Map.find_opt x env with
    | Some x -> x
    | None ->
      prerr_endline ("WARNING: Key error: Typing.get " ^ Variable.name x);
      Index.Independent

  let get_opt : Variable.t -> t -> Index.t option = Variable.Map.find_opt

  let put : Variable.t -> Index.t -> t -> t = Variable.Map.add

  let add_i (x: Variable.t) : t -> t =
    put x Index.Independent

  let add_d (x: Variable.t) : t -> t =
    put x Index.Dependent

  let update (key: Variable.t) (v:Index.t option) : t -> t =
    Variable.Map.update key (fun _ -> v)


end


let rec types_exp (env:Typing.t) (e:D_lang.Expr.t) : (Typing.t * Index.t) =
  let ret (l:D_lang.Expr.t list) : Typing.t * Index.t =
    types_exp_list env l
  in
  match e with
  | ParmVarDecl {name=x}
  | VarDecl {name=x} -> (env, Typing.get x env)

  | BinaryOperator b ->
    let x = D_lang.Expr.to_variable b.lhs in
    (match x, b.opcode = "=" with
    | Some x, true -> 
      let (env, a) = types_exp env b.rhs in
      (Typing.put x a env, a)
    | _, _ ->
      ret [ b.lhs; b.rhs ])

  | CallExpr c ->
    ret (c.func :: c.args)

  | CXXOperatorCallExpr c ->
    ret (c.func :: c.args)

  | CXXConstructExpr c ->
    ret c.args

  | ConditionalOperator c ->
    ret [ c.cond; c.then_expr; c.else_expr ]
  
  | MemberExpr {base=e; _}
  | CXXNewExpr {arg=e} 
  | CXXDeleteExpr {arg=e} 
  | UnaryOperator {child=e; _} -> types_exp env e

  | SizeOfExpr _
  | RecoveryExpr _
  | EnumConstantDecl _
  | UnresolvedLookupExpr _
  | NonTypeTemplateParmDecl _
  | FunctionDecl _
  | CXXMethodDecl _
  | CharacterLiteral _
  | FloatingLiteral _
  | IntegerLiteral _
  | CXXBoolLiteralExpr _ -> (env, Index.Independent)


and types_exp_list (env:Typing.t) (l: D_lang.Expr.t list) : Typing.t * Index.t =
  match l with
  | [] -> (env, Index.Independent)
  | e :: es ->
    let (env, a1) = types_exp env e in
    let (env, a2) = types_exp_list env es in
    (env, Index.add a1 a2)

module Stmt = struct
  type t =
    | NoAccesses
    | Has of {data: Index.t; control: Index.t}

  let no_access : t = NoAccesses

  let has_access (d:Index.t) : t =
    Has { control = Index.Independent; data = d}

  let add_control (s:t) (c:Index.t) : t =
    match s with
    | NoAccesses -> NoAccesses
    | Has s -> Has {s with control = Index.add s.control c}

  let add (lhs:t) (rhs:t) : t =
    match lhs, rhs with
    | NoAccesses, x
    | x, NoAccesses -> x
    | Has lhs, Has rhs ->
      Has {
        control = Index.add lhs.control rhs.control;
        data = Index.add lhs.data rhs.data;
      }

  let to_string: t -> string =
    function
    | NoAccesses -> "ind,ind"
    | Has {data=d; control=c} ->
      let d = match d with
      | Dependent -> "data"
      | Independent -> "ind"
      in
      let c = match c with
      | Dependent -> "ctrl"
      | Independent -> "ind"
      in
      d ^ "," ^ c 

end

let types_init (env:Typing.t) (e:Init.t) : Typing.t * Index.t =
  let open Index in
  match e with
  | CXXConstructExpr _ -> (env, Independent)
  | InitListExpr l -> types_exp_list env l.args
  | IExpr e -> types_exp env e

let rec types_stmt (env:Typing.t) (s:D_lang.Stmt.t) : Typing.t * Stmt.t =
  match s with
  | WriteAccessStmt d ->
    (* Abort if any index is dependent *)
    let (env, ty) = types_exp_list env d.target.index in
    (env, Stmt.has_access ty)

  | ReadAccessStmt s ->
    (* Abort if any index is dependent *)
    let (env, ty) = types_exp_list env s.source.index in
    (Typing.put s.target Index.Dependent env, Stmt.has_access ty)

  | DeclStmt (d::is) ->
    let x = d.ty_var.name in
    let (env, ty) = match d.init with
      | Some i -> types_init env i
      | None -> (env, Index.Independent)
    in
    types_stmt (Typing.put x ty env) (DeclStmt is)

  | IfStmt s ->
    let (env, ty) = types_exp env s.cond in
    let (env1, ty1) = types_stmt env s.then_stmt in
    let (env2, ty2) = types_stmt env s.else_stmt in
    let ty1 = Stmt.add_control ty1 ty in
    let ty2 = Stmt.add_control ty2 ty in
    (Typing.add env1 env2, Stmt.add ty1 ty2)

  | ForStmt s ->
    let orig_env = env in
    (* 1. Abort if any variable used in the loop range is dependent *)
    let (env, ctl) = types_exp_list env (D_lang.for_to_expr s) in
    (* 2. Get all loop variables being "declared" *)
    let xs = for_loop_vars s in
    (* 3. Get the old values of each loop variable (if any) *)
    let old = List.map (fun x -> Typing.get_opt x env) xs in
    (* 4. Mark each loop variable as independent *)
    let env = List.fold_left (fun e x -> Typing.add_i x e) env xs in
    (* 5. Typecheck the loop body *)
    let (env, ty) = types_stmt env s.body in
    (* 6. Undo the bindings of the loop variables *)
    let bindings = Common.zip xs old in
    let env = List.fold_left (fun e (x,o) -> Typing.update x o e) env bindings in
    (* 7. Merge the original with the final one *)
    (env |> Typing.add orig_env, Stmt.add_control ty ctl)

  | SExpr _
  | DeclStmt []
  | BreakStmt
  | GotoStmt
  | ContinueStmt
  | ReturnStmt -> (env, Stmt.no_access)
  

  | CaseStmt {body=s}
  | SwitchStmt {body=s}
  | DefaultStmt s
  | WhileStmt {body=s}
  | DoStmt {body=s} -> types_stmt env s
  
  | CompoundStmt s ->
    types_stmt_list env s





and types_stmt_list (env:Typing.t) (s:D_lang.Stmt.t list) : Typing.t * Stmt.t =
  match s with
  | [] -> (env, Stmt.no_access)
  | s :: ss ->
    let (env, ty1) = types_stmt env s in
    let (env, ty2) = types_stmt_list env ss in
    (env, Stmt.add ty1 ty2)

let types_kernel (k:Kernel.t) : Stmt.t =
  (* Initialize the environment: each parameter is independent *)
  let env =
    k.params
    |> List.map (fun (p:C_lang.Param.t) -> p.ty_var.name)
    |> List.fold_left (fun env x -> Typing.add_i x env) Typing.make
  in
  types_stmt env k.code |> snd

let types_def : d_def -> (string * Stmt.t) option =
  function
  | Declaration _ -> None
  | Kernel k -> Some (k.name, types_kernel k)

let types_program : d_program -> (string * Stmt.t) list =
  List.filter_map types_def
