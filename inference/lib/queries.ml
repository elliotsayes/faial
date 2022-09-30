open Stage0
open Stage1

module StringMap = Common.StringMap
module VarSet = Variable.Set
module VarMap = Variable.Map
type json = Yojson.Basic.t

module Variables = struct
  open C_lang
  let flatten_member (e:Expr.t) : Expr.t =
    let open Expr.Visit in
    e |> map (
      function
      | MemberExpr {base=VarDecl {name=x; ty=ty}; name=f} ->
        let x = Variable.update_name (fun n -> n ^ "." ^ f) x in
        ParmVarDecl {name=x; ty=ty}
      | e' -> e'
    )

  let is_thread_idx (v:c_var) : bool =
    let v : string =
      v.name
      |> Variable.name
    in
    v = "threadIdx.x" || v = "threadIdx.y" || v = "threadIdx.z"

  (* Returns all variables present in an expression *)
  let from_expr (e:Expr.t) : c_var Seq.t =
    let open Expr.Visit in
    e |> fold (function
      | ParmVarDecl v
      | VarDecl v
        -> Seq.return v

      | CXXBoolLiteral _
      | SizeOf _
      | Recovery _
      | CharacterLiteral _
      | CXXMethodDecl _
      | FloatingLiteral _
      | FunctionDecl _
      | IntegerLiteral _
      | NonTypeTemplateParmDecl _
      | EnumConstantDecl _
      | UnresolvedLookup _
      -> Seq.empty

      | UnaryOperator {child=e}
      | Member {base=e}
      | CXXNew {arg=e}
      | CXXDelete {arg=e}
      -> e

      | ArraySubscript {lhs=s1; rhs=s2}
      | BinaryOperator {lhs=s1; rhs=s2}
      -> Seq.append s1 s2

      | ConditionalOperator e ->
        e.cond
        |> Seq.append e.then_expr
        |> Seq.append e.else_expr

      | CXXOperatorCall {func=a; args=l}
      | Call {func=a; args=l} ->
        List.to_seq l
        |> Seq.concat
        |> Seq.append a

      | CXXConstruct l ->
        List.to_seq l.args |> Seq.concat
    )

  (* Given a sequence of c_var, generate a set of variables *)
  let to_set (s:c_var Seq.t) : VarSet.t =
    s
    |> Seq.map (fun (x:c_var) -> x.name)
    |> List.of_seq
    |> VarSet.of_list

  let summarize (s:Stmt.t) : json =
    let vars =
      s
      (* Get all sequences *)
      |> Stmt.Visit.to_expr_seq
      (* Convert threadIdx.x to var *)
      |> Seq.map flatten_member
      (* Get all variables *)
      |> Seq.concat_map from_expr
      (* Keep threadIdx.x *)
      |> Seq.filter is_thread_idx
      |> to_set
      |> VarSet.elements
      |> List.map Variable.name
      |> List.map (fun x -> `String x)
    in
    `Assoc [
      "tids", `List vars
    ]

end


module Calls = struct
  open C_lang

  type t = {func: Expr.t; args: Expr.t list}

  (* Returns all function calls in a statement, as
        a sequence. *)
  let to_seq (c:Stmt.t) : t Seq.t =
    (** Returns all function calls in an expression, as
        a sequence. *)
    let rec to_seq (e:Expr.t) : t Seq.t =
      match e with
      (* Found a function call *)
      | CallExpr {func=f; args=a}
      | CXXOperatorCallExpr {func=f; args=a}
      -> Seq.return {func=f; args=a}

      | CXXBoolLiteralExpr _
      | SizeOfExpr _
      | RecoveryExpr _
      | CharacterLiteral _
      | CXXMethodDecl _
      | FloatingLiteral _
      | FunctionDecl _
      | IntegerLiteral _
      | NonTypeTemplateParmDecl _
      | ParmVarDecl _
      | VarDecl _
      | EnumConstantDecl _
      | UnresolvedLookupExpr _
      -> Seq.empty

      | UnaryOperator {child=e}
      | MemberExpr {base=e}
      | CXXNewExpr {arg=e}
      | CXXDeleteExpr {arg=e}
      -> to_seq e

      | ArraySubscriptExpr {lhs=s1; rhs=s2}
      | BinaryOperator {lhs=s1; rhs=s2}
      -> Seq.append (to_seq s1) (to_seq s2)

      | ConditionalOperator e ->
        to_seq e.cond
        |> Seq.append (to_seq e.then_expr)
        |> Seq.append (to_seq e.else_expr)

      | CXXConstructExpr l ->
        List.to_seq l.args
        |> Seq.concat_map to_seq
    in
    (* Use an expression iterator, extract function
       calls for each expression therein. *)
    Stmt.Visit.to_expr_seq c
    |> Seq.concat_map to_seq

  let calls_using_array (c: Stmt.t) : t Seq.t =
    to_seq c
    |> Seq.filter (fun c ->
      c.args
      |> List.exists (fun e ->
        Expr.to_type e
        |> parse_type
        |> Result.map C_type.is_array
        |> Rjson.unwrap_or false
      )
    )

  let count (c:Stmt.t) : int StringMap.t =
    to_seq c
    (* Only get function calls that have a function name
       in the position of the function *)
    |> Seq.concat_map (fun c ->
      match c.func with
      | FunctionDecl x -> Seq.return (Variable.name x.name)
      | _ -> Seq.empty
    )
    (* Count how many times each name is used *)
    |> Seq.fold_left (fun wc name ->
      wc |> StringMap.update name (function
        | Some n -> Some (n + 1)
        | None -> Some 1
      )
    ) StringMap.empty

  (* Returns true if it contains thread synchronization *)
  let has_sync (c:Stmt.t) : bool =
    count c |> StringMap.mem "__syncthreads"

  let summarize (*global_arrays:VarSet.t*) (s:Stmt.t) : json =
    let func_count : (string * json) list = count s
    |> StringMap.bindings
    |> List.map (fun (k,v) -> k, `Int v)
    in
    `Assoc [
      "function count", `Assoc func_count;
    ]

end


(* Serializes a set of variables as a list of strings *)
let var_set_to_json (vars:VarSet.t) : json =
  let vars = vars
  |> VarSet.elements
  |> List.map (fun x -> `String (Variable.name x))
  in
  `List vars

(* Performs loop analysis. *)
module Loops = struct
  open C_lang
  (* Represents a nesting of loops within other loops *)
  type loop =
    | While of {
        cond: Expr.t;
        body: loop list;
        data: Stmt.t;
      }
    | For of {
        init: ForInit.t option;
        cond: Expr.t option;
        inc: Expr.t option;
        body: loop list;
        data: Stmt.t;
      }
    | Do of {
        cond: Expr.t;
        body: loop list;
        data: Stmt.t;
      }

  type t = loop list

  let make : Stmt.t -> t =
    let rec to_seq (c:Stmt.t) : t =
      match c with
      | WhileStmt w ->
        [While {cond=w.cond; body=to_seq w.body; data=w.body}]
      | DoStmt w ->
        [Do {cond=w.cond; body=to_seq w.body; data=w.body}]
      | ForStmt w ->
        [For {init=w.init;
          cond=w.cond;
          inc=w.inc;
          body=to_seq w.body;
          data=w.body;
        }]
      | BreakStmt
      | GotoStmt
      | ReturnStmt
      | ContinueStmt
      | DeclStmt _
      | SExpr _
      -> []
      | IfStmt s ->
        to_seq s.then_stmt @ to_seq s.else_stmt
      | CompoundStmt l ->
        List.concat_map to_seq l
      | DefaultStmt s
      | SwitchStmt {body=s}
      | CaseStmt {body=s}
        -> to_seq s
    in
    to_seq

  let to_string (s: t) : string =
    let open Serialize in
    let rec stmt_to_s : loop -> PPrint.t list =
      function
      | For f -> [
          Line ("@for (" ^
              C_lang.ForInit.opt_to_string f.init ^ "; " ^
              Expr.opt_to_string f.cond ^ "; " ^
              Expr.opt_to_string f.inc ^ ") {"
          );
          Block(List.concat_map stmt_to_s f.body);
          Line ("}");
        ]
      | While {cond=b; body=s} -> [
          Line ("@while (" ^ C_lang.Expr.to_string b ^ ") {");
          Block (List.concat_map stmt_to_s s);
          Line "}"
        ]
    | Do {cond=b; body=s} -> [
        Line "}";
        Block (List.concat_map stmt_to_s s);
        Line ("@do (" ^ Expr.to_string b ^ ") {");
      ]
    in
    List.concat_map stmt_to_s s |> PPrint.doc_to_string

  let max_depth: t -> int =
    let rec depth1 (x: loop) : int =
      match x with
      | For {body=s}
      | While {body=s}
      | Do {body=s}
        -> 1 + depth s
    and depth (l: t) : int =
      List.fold_left (fun d e -> max d (depth1 e)) 0 l
    in
    depth

  let filter (keep:loop -> bool) : t -> t =
    let rec filter (l:t) : t =
      l |> Common.map_opt (fun (e:loop) ->
        if keep e then
          Some (filter1 e)
        else None
      )
    and filter1 (x:loop) : loop =
      match x with
      | While w -> While { w with body = filter w.body }
      | For f -> For { f with body = filter f.body }
      | Do d -> Do { d with body = filter d.body }
    in
    filter

  let data : loop -> Stmt.t =
    function
    | For {data=s}
    | While {data=s}
    | Do {data=s}
      -> s

  (* Given set of loops, filters out unsynchronized loops. *)
  let filter_sync : t -> t =
    filter (fun l -> data l |> Calls.has_sync )

  let filter_using_loop_vars : t -> t =
    let rec filter1 (bound:VarSet.t) : loop -> loop option =
      function
      | For f ->
        (* Get the variables being defined in the init section *)
        let loop_vars =
          f.init
          |> Option.map ForInit.loop_vars
          |> Ojson.unwrap_or []
          |> VarSet.of_list
        in
        (* Check if an expression uses a bound variable *)
        let uses_bound (e:Expr.t) : bool =
          (* Range over all variables used in e *)
          Variables.from_expr e
          |> Seq.exists (fun (x:c_var) ->
            VarSet.mem x.name bound
          )
        in
        let in_init =
          match f.init with
          | Some i ->
            ForInit.to_expr_seq i
            |> Seq.exists uses_bound
          | None -> false
        in
        let in_cond =
          match f.cond with
          | Some c -> uses_bound c
          | None -> false
        in
        let bound = VarSet.union loop_vars bound in
        let body = filter bound f.body in
        if in_init || in_cond || not (Common.list_is_empty body) then
          Some (For { f with body=body })
        else
          None
      | Do d ->
        let b = filter bound d.body in
        if Common.list_is_empty b then
          None
        else
          Some (Do { d with body=b })

      | While w ->
        let b = filter bound w.body in
        if Common.list_is_empty b then
          None
        else
          Some (While {w with body=b})
      and filter (bound:VarSet.t) (s: t) : t =
        Common.map_opt (filter1 bound) s
    in
    filter VarSet.empty

  let summarize (s:Stmt.t) : json =
    let l = make s in
    `Assoc [
        "# of loops using loop variables in bounds", `Int (l |> filter_using_loop_vars |> List.length);
        "max loop depth", `Int (l |> max_depth);
        "max sync-loop depth", `Int (l |> filter_sync |> max_depth);
    ]
end

module MutatedVar = struct
  open C_lang
  (* Checks if a variable is updated inside a conditional/loop *)
  (* Keeps track of the scope level a variable was defined *)

  let rec get_writes (e:Expr.t) (writes:VarSet.t) : VarSet.t =
    match e with
    | BinaryOperator {lhs=ParmVarDecl{name=x}; opcode="="; rhs=s2; ty=ty}
    | BinaryOperator {lhs=VarDecl{name=x}; opcode="="; rhs=s2; ty=ty}
    ->
      let is_int =
        match parse_type ty with
        | Ok ty -> C_type.is_int ty
        | Error _ -> false
      in
      let w = if is_int then VarSet.add x writes else writes in
      get_writes s2 w

    | CallExpr {func=f; args=a}
    | CXXOperatorCallExpr {func=f; args=a}
    -> f::a |> List.fold_left (fun writes e -> get_writes e writes) writes

    | ParmVarDecl _
    | VarDecl _
    | CXXBoolLiteralExpr _
    | SizeOfExpr _
    | RecoveryExpr _
    | CharacterLiteral _
    | CXXMethodDecl _
    | FloatingLiteral _
    | FunctionDecl _
    | IntegerLiteral _
    | NonTypeTemplateParmDecl _
    | EnumConstantDecl _
    | UnresolvedLookupExpr _
    -> writes

    | UnaryOperator {child=e}
    | MemberExpr {base=e}
    | CXXNewExpr {arg=e}
    | CXXDeleteExpr {arg=e}
    -> get_writes e writes

    | BinaryOperator {lhs=s1; rhs=s2}
    | ArraySubscriptExpr {lhs=s1; rhs=s2}
    ->
      writes
      |> get_writes s1
      |> get_writes s2

    | ConditionalOperator e ->
      writes
      |> get_writes e.cond
      |> get_writes e.then_expr
      |> get_writes e.else_expr

    | CXXConstructExpr {args=l} ->
       List.fold_left (fun writes e -> get_writes e writes) writes l

  let typecheck (s: Stmt.t) : VarSet.t =
    let rec typecheck (scope:int) (env:int VarMap.t) : Stmt.t -> int VarMap.t * VarSet.t =
      let typecheck_e ?(scope=scope) (e:Expr.t) : VarSet.t =
        get_writes e VarSet.empty
        |> VarSet.filter (fun x ->
          match VarMap.find_opt x env with
          | Some n -> scope > n
          | None -> false
        )
      in
      let typecheck_o ?(scope=scope) (e:Expr.t option) : VarSet.t =
        match e with
        | Some e -> typecheck_e ~scope:scope e
        | None -> VarSet.empty
      in
      let typecheck_f ?(scope=scope) (e:ForInit.t option) : VarSet.t =
        match e with
        | Some (Decls l) ->
          Stmt.Visit.to_expr_seq (DeclStmt l)
          |> Seq.fold_left (fun vars e ->
              VarSet.union vars (typecheck_e ~scope e)
          ) VarSet.empty
        | Some (Expr e) -> typecheck_e ~scope:scope e
        | None -> VarSet.empty
      in
      function
        | DeclStmt l ->
          let env2 =
            l
            |> List.map (fun x -> Decl.(x.name, scope))
            |> Variable.MapUtil.from_list in
          let env = VarMap.union (fun k e1 e2 ->
            Some (max e1 e2)
          ) env2 env
          in
          (env, VarSet.empty)
        | DoStmt {cond=e; body=s}
        | WhileStmt {cond=e; body=s}
        ->
          let (env, vars) = typecheck (scope + 1) env s in
          (env, VarSet.union (typecheck_e e) vars)

        | SExpr e ->
          (env, typecheck_e e)

        | ForStmt w ->
          let (env, vars) = typecheck (scope + 1) env w.body in
          let vars =
            vars
            |> VarSet.union (typecheck_f ~scope:(scope + 1) w.init)
            |> VarSet.union (typecheck_o ~scope:(scope + 1) w.cond)
            |> VarSet.union (typecheck_o ~scope:(scope + 1) w.inc)
          in
          (env, vars)

        | BreakStmt
        | GotoStmt
        | ReturnStmt
        | ContinueStmt
        -> (env, VarSet.empty)

        | IfStmt s ->
          let (_, vars1) = typecheck (scope + 1) env s.else_stmt in
          let (_, vars2) = typecheck (scope + 1) env s.then_stmt in
          let vars =
            VarSet.union vars1 vars2
            |> VarSet.union (typecheck_e s.cond)
          in
          (env, vars)

        | CompoundStmt l ->
          List.fold_left (fun (env, vars1) s ->
            let (env, vars2) = typecheck scope env s in
            (env, VarSet.union vars1 vars2)
          ) (env, VarSet.empty) l

        | DefaultStmt s
        | SwitchStmt {body=s}
        | CaseStmt {body=s}
          -> typecheck (scope + 1) env s
    in
    typecheck 0 VarMap.empty s |> snd

  let summarize (s: Stmt.t) : json =
    typecheck s
    |> var_set_to_json
end


module Declarations = struct
  open C_lang
  let to_seq (s:Stmt.t) : Decl.t Seq.t =
    s |> Stmt.Visit.fold (function
      | For {init=Some (ForInit.Decls l); body=s} ->
        List.to_seq l
        |> Seq.append s

      | Decl l -> List.to_seq l

      | Break
      | Goto
      | Return
      | Continue
      | SExpr _
        -> Seq.empty

      | Compound l -> List.to_seq l |> Seq.concat

      | If {then_stmt=s1; else_stmt=s2} -> Seq.append s1 s2

      | Do {body=s}
      | Switch {body=s}
      | While {body=s}
      | Default s
      | Case {body=s}
      | For {init=Some (ForInit.Expr _); body=s}
      | For {init=None; body=s}
        -> s
    )

  let shared_arrays (s: Stmt.t) : VarSet.t =
    to_seq s
    |> Seq.filter Decl.is_shared
    |> Seq.map Decl.to_c_var
    |> Variables.to_set

  let summarize (s: Stmt.t) : json =
    let s = to_seq s in
    let all_count =
      s
      |> Seq.length
    in
    let int_count =
      s
      |> Seq.filter (Decl.has_type C_type.is_int)
      |> Seq.length
    in
    let shared_arrays =
      s
      |> Seq.filter Decl.is_shared
      |> Seq.map Decl.to_c_var
      |> Variables.to_set
      |> var_set_to_json
    in
    `Assoc [
      "# of local shared arrays", shared_arrays;
      "# of decls", `Int all_count;
      "# of integer decls", `Int int_count;
    ]
end

module Conditionals = struct
  open C_lang
  type t = {cond: Expr.t; then_stmt: Stmt.t; else_stmt: Stmt.t}
  let rec to_seq : Stmt.t -> t Seq.t =
    function
    | IfStmt {cond=c; then_stmt=s1; else_stmt=s2} ->
      Seq.return {cond=c; then_stmt=s1; else_stmt=s2}

    | DeclStmt _
    | BreakStmt
    | GotoStmt
    | ReturnStmt
    | ContinueStmt
    | SExpr _
      ->
      Seq.empty

    | CompoundStmt l -> List.to_seq l |> Seq.concat_map to_seq

    | WhileStmt {body=s}
    | ForStmt {body=s}
    | DoStmt {body=s}
    | SwitchStmt {body=s}
    | DefaultStmt s
    | CaseStmt {body=s}
      ->
      to_seq s

  let summarize (s:Stmt.t) : json =
    let count =
      to_seq s
      |> Seq.filter (fun x ->
        Calls.has_sync x.then_stmt || Calls.has_sync x.else_stmt
      )
      |> Seq.length
    in
    `Assoc [
      "# of ifs", `Int (to_seq s |> Seq.length);
      "# of synchronized ifs", `Int count;
    ]
end

module ForEach = struct
  (* Loop inference *)
  open D_lang

  type t =
    | For of Stmt.d_for
    | While of {cond: Expr.t; body: Stmt.t}
    | Do of {cond: Expr.t; body: Stmt.t}

  let to_string : t -> string =
    function
    | For f ->
      "for (" ^
      ForInit.opt_to_string f.init ^ "; " ^
      Expr.opt_to_string f.cond ^ "; " ^
      Expr.opt_to_string f.inc ^
      ")"
    | While {cond=b} -> "while (" ^ Expr.to_string b ^ ")"
    | Do {cond=b} -> "do (" ^ Expr.to_string b ^ ")"

  (* Search for all loops available *)
  let rec to_seq: Stmt.t -> t Seq.t =
    function
    | ForStmt d ->
      Seq.return (For d)
    | WhileStmt {body=s; cond=c} ->
      Seq.return (While {body=s; cond=c})
    | DoStmt {body=s; cond=c} ->
      Seq.return (Do {body=s; cond=c})
    | WriteAccessStmt _
    | ReadAccessStmt _
    | BreakStmt
    | GotoStmt
    | ContinueStmt
    | ReturnStmt
    | DeclStmt _
    | SExpr _
      -> Seq.empty
    | IfStmt {then_stmt=s1; else_stmt=s2}
      -> to_seq s1 |> Seq.append (to_seq s2)
    | SwitchStmt {body=s}
    | DefaultStmt s
    | CaseStmt {body=s}
      -> to_seq s
    | CompoundStmt l ->
      List.to_seq l
      |> Seq.concat_map to_seq

  let infer (s: Stmt.t) : (t * Exp.range option) Seq.t =
    to_seq s
    |> Seq.map (function
      | For r ->
        let o = match D_to_imp.infer_range r with
        | Ok (Some r) -> Some r
        | _ -> None
        in
        (For r, o)
      | l -> (l, None)
    )

  let summarize (s:Stmt.t) : json =
    let elems = infer s |> List.of_seq in
    let count = List.length elems in
    let found =
      elems
      |> List.filter (fun (_, o) -> Option.is_some o)
      |> List.length
    in
    let missing = elems |> Common.map_opt (function
      | (x, None) -> Some (`String (to_string x))
      | (x, Some _) -> None
    ) in
    `Assoc [
      "total", `Int count;
      "inferred", `Int found;
      "missing", `List missing;
    ]
end
