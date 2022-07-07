open Dlang

module Index = struct
  type t =
    | Dependent
    | Independent

  let add (t1:t) (t2:t) : t =
    match t1, t2 with
    | Independent, Independent -> Independent
    | _, _ -> Dependent

  let sum : t list -> t = List.fold_left add Independent
end

module Typing = struct
  type t = Index.t Exp.VarMap.t

  let make : t = Exp.VarMap.empty

  let add : t -> t -> t =
    Exp.VarMap.union (fun k v1 v2 ->
      Some (Index.add v1 v2)
    )

  let get (x: Exp.variable) (env:t) : Index.t =
    match Exp.VarMap.find_opt x env with
    | Some x -> x
    | None ->
      prerr_endline ("WARNING: Key error: Typing.get " ^ Exp.var_name x);
      Index.Independent

  let get_opt : Exp.variable -> t -> Index.t option = Exp.VarMap.find_opt

  let put : Exp.variable -> Index.t -> t -> t = Exp.VarMap.add

  let add_i (x: Exp.variable) : t -> t =
    put x Index.Independent

  let add_d (x: Exp.variable) : t -> t =
    put x Index.Dependent

  let update (key: Exp.variable) (v:Index.t option) : t -> t =
    Exp.VarMap.update key (fun _ -> v)


end


let rec types_exp (env:Typing.t) (e:Dlang.d_exp) : (Typing.t * Index.t) =
  let ret (l:Dlang.d_exp list) : Typing.t * Index.t =
    types_exp_list env l
  in
  let open Dlang in
  match e with
  | ParmVarDecl {name=x}
  | VarDecl {name=x} -> (env, Typing.get x env)

  | BinaryOperator b ->
    let x = Dlang.get_variable b.lhs in
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
  | UnaryOperator {child=e; _} -> types_exp env e

  | EnumConstantDecl _
  | UnresolvedLookupExpr _
  | NonTypeTemplateParmDecl _
  | FunctionDecl _
  | CXXMethodDecl _
  | CharacterLiteral _
  | FloatingLiteral _
  | IntegerLiteral _
  | CXXBoolLiteralExpr _ -> (env, Index.Independent)


and types_exp_list (env:Typing.t) (l: d_exp list) : Typing.t * Index.t =
  match l with
  | [] -> (env, Index.Independent)
  | e :: es ->
    let (env, a1) = types_exp env e in
    let (env, a2) = types_exp_list env es in
    (env, Index.add a1 a2)

type s_error = string StackTrace.t

let print_s_error : s_error -> unit =
  StackTrace.iter prerr_endline

let s_error_to_buffer (e: s_error) : Buffer.t =
  let b = Buffer.create 512 in
  StackTrace.iter (Buffer.add_string b) e;
  b

type 'a s_result = ('a, s_error) Result.t

let s_root_cause (msg:string) : 'a s_result =
  Error (RootCause msg)

(* Monadic let *)
let (let*) = Result.bind
(* Monadic pipe *)
let (>>=) = Result.bind

let ensure_i_exp (env:Typing.t) (e:d_exp) (msg:string) : Typing.t s_result =
  let (env, a) = types_exp env e in
  if a = Index.Dependent then
    s_root_cause (msg ^  ": " ^ Dlang.exp_to_s e)
  else Ok env

let ensure_i_exp_list (env:Typing.t) (es:d_exp list) (msg:string) : Typing.t s_result =
  let (env, a) = types_exp_list env es in
  if a = Index.Dependent then
    s_root_cause (msg ^  ": " ^ Dlang.list_to_s Dlang.exp_to_s es)
  else Ok env

let types_init (env:Typing.t) (e:d_init) : Typing.t * Index.t =
  let open Index in
  match e with
  | CXXConstructExpr _ -> (env, Independent)
  | InitListExpr l -> types_exp_list env l.args
  | IExp e -> types_exp env e

let rec types_stmt (env:Typing.t) (s:d_stmt) : Typing.t s_result =
  match s with
  | IfStmt s ->
    let* env = ensure_i_exp env s.cond "if" in
    let* env1 = types_stmt env s.then_stmt in
    let* env2 = types_stmt env s.else_stmt in
    Ok (Typing.add env1 env2)

  | ReadAccessStmt s ->
    let* env = ensure_i_exp_list env s.source.index "read" in
    Ok (Typing.put s.target Index.Dependent env)

  | SExp _
  | DeclStmt []
  | BreakStmt
  | GotoStmt
  | ContinueStmt
  | ReturnStmt -> Ok env
  

  | CaseStmt {body=s}
  | SwitchStmt {body=s}
  | DefaultStmt s
  | WhileStmt {body=s}
  | DoStmt {body=s} -> types_stmt env s
  | WriteAccessStmt d ->

    let* env = ensure_i_exp_list env d.target.index "write" in
    Ok env
  
  | CompoundStmt s ->
    types_stmt_list env s

  | ForStmt s ->
    let orig_env = env in
    (* 1. Make sure that all variables used in the loop range are independent *)
    let* env = ensure_i_exp_list env (for_to_exp s) "for.range" in
    (* 2. Get all loop variables being "declared" *)
    let xs = for_loop_vars s in
    (* 3. Get the old values of each loop variable (if any) *)
    let old = List.map (fun x -> Typing.get_opt x env) xs in
    (* 4. Mark each loop variable as independent *)
    let env = List.fold_left (fun e x -> Typing.add_i x e) env xs in
    (* 5. Typecheck the loop body *)
    let* env = types_stmt env s.body in
    (* 6. Undo the bindings of the loop variables *)
    let bindings = Common.zip xs old in
    let env = List.fold_left (fun e (x,o) -> Typing.update x o e) env bindings in
    (* 7. Merge the original with the final one *)
    Ok (env |> Typing.add orig_env)

  | DeclStmt (d::is) ->
    let x = d.name in
    let (env, ty) = match d.init with
      | Some i -> types_init env i
      | None -> (env, Index.Independent)
    in
    types_stmt (Typing.put x ty env) (DeclStmt is)




and types_stmt_list (env:Typing.t) (s:d_stmt list) : Typing.t s_result =
  match s with
  | [] -> Ok env
  | s :: ss ->
    let* env = types_stmt env s in
    types_stmt_list env ss

let types_kernel (k:d_kernel) : Typing.t s_result =
  let env =
    k.params
    |> List.map (fun (p:Cast.c_param) -> p.name)
    |> List.fold_left (fun env x -> Typing.add_i x env) Typing.make
  in
  types_stmt env k.code

let types_def : d_def -> (string * (Typing.t s_result)) option =
  function
  | Declaration _ -> None
  | Kernel k -> Some (k.name, types_kernel k)

let types_program : d_program -> (string * Typing.t s_result) list =
  Common.map_opt types_def
