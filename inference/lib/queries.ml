open Stage0
open Protocols

module StringMap = Common.StringMap
module VarSet = Variable.Set
module VarMap = Variable.Map
type json = Yojson.Basic.t

let var_list_to_json (vars:Variable.t list) : json =
  let vars =
    List.map (fun x -> `String (Variable.name x)) vars
  in
  `List vars

(* Serializes a set of variables as a list of strings *)
let var_set_to_json (vars:VarSet.t) : json =
  vars |> VarSet.elements |> var_list_to_json

module Params = struct
  open C_lang
  let summarize (k:Kernel.t) : json =
    let filter_params (pred:C_type.t -> bool) : json =
      k.params
      |> List.filter (Param.has_type pred)
      |> List.map Param.name
      |> var_list_to_json
    in
    let global_arrays = filter_params C_type.is_array in
    let global_int = filter_params C_type.is_int in
    `Assoc [
      "global arrays", global_arrays;
      "integers", global_int;
      "total", `Int (List.length k.params);
    ]
end

module GlobalDeclArrays = struct
  open C_lang
  let summarize (ds: Decl.t list) : json =
    let ds = List.filter Decl.is_array ds in
    let ds = List.filter Decl.is_shared ds in
    `Assoc [
      "shared arrays", List.map Decl.var ds |> var_list_to_json;
    ]
end

module Variables = struct
  open C_lang
  let flatten_member (e:Expr.t) : Expr.t =
    let open Expr.Visit in
    e |> map (
      function
      | MemberExpr {base=VarDecl {name=x; ty=ty}; name=f; _} ->
        let x = Variable.update_name (fun n -> n ^ "." ^ f) x in
        ParmVarDecl {name=x; ty=ty}
      | e' -> e'
    )

  (* Returns all variables present in an expression *)
  let from_expr (e:Expr.t) : TyVariable.t Seq.t =
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

      | UnaryOperator {child=e; _}
      | Member {base=e; _}
      | CXXNew {arg=e; _}
      | CXXDelete {arg=e; _}
      -> e

      | ArraySubscript {lhs=s1; rhs=s2; _}
      | BinaryOperator {lhs=s1; rhs=s2; _}
      -> Seq.append s1 s2

      | ConditionalOperator e ->
        e.cond
        |> Seq.append e.then_expr
        |> Seq.append e.else_expr

      | CXXOperatorCall {func=a; args=l; _}
      | Call {func=a; args=l; _} ->
        List.to_seq l
        |> Seq.concat
        |> Seq.append a

      | CXXConstruct l ->
        List.to_seq l.args |> Seq.concat
    )

  (* Given a sequence of c_var, generate a set of variables *)
  let to_set (s:TyVariable.t Seq.t) : VarSet.t =
    s
    |> Seq.map (fun (x:TyVariable.t) -> x.name)
    |> List.of_seq
    |> VarSet.of_list

  let summarize (s:Stmt.t) : json =
    let tids =
      s
      (* Get all sequences *)
      |> Stmt.Visit.to_expr_seq
      (* Convert threadIdx.x to var *)
      |> Seq.map flatten_member
      (* Get all variables *)
      |> Seq.concat_map from_expr
      (* Keep threadIdx.x *)
      |> Seq.filter TyVariable.is_tid
      |> to_set
      |> var_set_to_json
    in
    `Assoc [
      "tids", tids
    ]

end

module Declarations = struct
  open C_lang
  let to_seq (s:Stmt.t) : Decl.t Seq.t =
    s |> Stmt.Visit.fold (function
      | For {init=Some (ForInit.Decls l); body=s; _} ->
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

      | If {then_stmt=s1; else_stmt=s2; _} -> Seq.append s1 s2

      | Do {body=s; _}
      | Switch {body=s; _}
      | While {body=s; _}
      | Default s
      | Case {body=s; _}
      | For {init=Some (ForInit.Expr _); body=s; _}
      | For {init=None; body=s; _}
        -> s
    )

  let shared_arrays (s: Stmt.t) : VarSet.t =
    to_seq s
    |> Seq.filter Decl.is_shared
    |> Seq.map Decl.ty_var
    |> Variables.to_set

  let summarize (s: Stmt.t) : json =
    let s = to_seq s in
    let all_count =
      s
      |> Seq.length
    in
    let int_count =
      s
      |> Seq.map Decl.ty_var
      |> Seq.filter (TyVariable.has_type C_type.is_int)
      |> Seq.length
    in
    let shared_arrays =
      s
      |> Seq.filter Decl.is_shared
      |> Seq.map Decl.ty_var
      |> Variables.to_set
      |> var_set_to_json
    in
    `Assoc [
      "local shared arrays", shared_arrays;
      "# of decls", `Int all_count;
      "# of integer decls", `Int int_count;
    ]
end

module Calls = struct
  open C_lang

  type t = {func: Expr.t; args: Expr.t list}

  let function_name (x:t) : Variable.t option =
    Expr.to_variable x.func

  let expressions (x:t) : Expr.t Seq.t =
    Seq.return x.func
    |> Seq.append (List.to_seq x.args)

  let variables (x:t) : TyVariable.t Seq.t =
    expressions x
    |> Seq.concat_map Variables.from_expr

  let uses_vars (vs:VarSet.t) (x:t) : bool =
    x
    |> variables
    |> Seq.exists (fun (x:TyVariable.t) -> VarSet.mem x.name vs)

  (* Returns all function calls in a statement, as
        a sequence. *)
  let to_seq (c:Stmt.t) : t Seq.t =
    (* Returns all function calls in an expression, as
        a sequence. *)
    let rec to_seq (e:Expr.t) : t Seq.t =
      match e with
      (* Found a function call *)
      | CallExpr {func=f; args=a; _}
      | CXXOperatorCallExpr {func=f; args=a; _}
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

      | UnaryOperator {child=e; _}
      | MemberExpr {base=e; _}
      | CXXNewExpr {arg=e; _}
      | CXXDeleteExpr {arg=e; _}
      -> to_seq e

      | ArraySubscriptExpr {lhs=s1; rhs=s2; _}
      | BinaryOperator {lhs=s1; rhs=s2; _}
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

  let summarize (arrays:Decl.t list) (k:Kernel.t) : json =
    let (shared, globals) = List.partition Decl.is_shared arrays in
    let uses_arr arrs =
      to_seq k.code
      |> Seq.filter (uses_vars arrs)
      |> Seq.filter_map function_name
      |> Seq.map (fun x -> `String (Variable.name x))
      |> List.of_seq
    in
    let uses_shared =
      Declarations.shared_arrays k.code
      |> VarSet.union (shared |> List.map Decl.var |> VarSet.of_list)
      |> uses_arr
    in
    let uses_global =
      k.params
      |> List.filter (Param.has_type C_type.is_array)
      |> List.map Param.ty_var
      |> List.to_seq
      |> Variables.to_set
      |> VarSet.union (globals |> List.map Decl.var |> VarSet.of_list)
      |> uses_arr
    in
    let func_count : (string * json) list =
      count k.code
      |> StringMap.bindings
      |> List.map (fun (k,v) -> k, `Int v)
    in
    `Assoc [
      "using shared arrays", `List uses_shared;
      "using global arrays", `List uses_global;
      "count", `Assoc func_count;
    ]

end

(* Performs loop analysis. *)
module NestedLoops = struct
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
      | SwitchStmt {body=s; _}
      | CaseStmt {body=s; _}
        -> to_seq s
    in
    to_seq

  let to_string (s: t) : string =
    let rec stmt_to_s : loop -> Indent.t list =
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
      | While {cond=b; body=s; _} -> [
          Line ("@while (" ^ C_lang.Expr.to_string b ^ ") {");
          Block (List.concat_map stmt_to_s s);
          Line "}"
        ]
    | Do {cond=b; body=s; _} -> [
        Line "}";
        Block (List.concat_map stmt_to_s s);
        Line ("@do (" ^ Expr.to_string b ^ ") {");
      ]
    in
    List.concat_map stmt_to_s s |> Indent.to_string

  let max_depth: t -> int =
    let rec depth1 (x: loop) : int =
      match x with
      | For {body=s; _}
      | While {body=s; _}
      | Do {body=s; _}
        -> 1 + depth s
    and depth (l: t) : int =
      List.fold_left (fun d e -> max d (depth1 e)) 0 l
    in
    depth

  let filter (keep:loop -> bool) : t -> t =
    let rec filter (l:t) : t =
      l |> List.filter_map (fun (e:loop) ->
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
    | For {data=s; _}
    | While {data=s; _}
    | Do {data=s; _}
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
          |> Seq.exists (fun (x:TyVariable.t) ->
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
        List.filter_map (filter1 bound) s
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
    | BinaryOperator {lhs=ParmVarDecl{name=x; _}; opcode="="; rhs=s2; ty=ty}
    | BinaryOperator {lhs=VarDecl{name=x; _}; opcode="="; rhs=s2; ty=ty}
    ->
      let is_int =
        match parse_type ty with
        | Ok ty -> C_type.is_int ty
        | Error _ -> false
      in
      let w = if is_int then VarSet.add x writes else writes in
      get_writes s2 w

    | CallExpr {func=f; args=a; _}
    | CXXOperatorCallExpr {func=f; args=a; _}
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

    | UnaryOperator {child=e; _}
    | MemberExpr {base=e; _}
    | CXXNewExpr {arg=e; _}
    | CXXDeleteExpr {arg=e; _}
    -> get_writes e writes

    | BinaryOperator {lhs=s1; rhs=s2; _}
    | ArraySubscriptExpr {lhs=s1; rhs=s2; _}
    ->
      writes
      |> get_writes s1
      |> get_writes s2

    | ConditionalOperator e ->
      writes
      |> get_writes e.cond
      |> get_writes e.then_expr
      |> get_writes e.else_expr

    | CXXConstructExpr {args=l; _} ->
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
            |> List.map (fun x -> (Decl.var x, scope))
            |> Variable.MapUtil.from_list in
          let env = VarMap.union (fun _ e1 e2 ->
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
        | SwitchStmt {body=s; _}
        | CaseStmt {body=s; _}
          -> typecheck (scope + 1) env s
    in
    typecheck 0 VarMap.empty s |> snd

  let summarize (s: Stmt.t) : json =
    typecheck s
    |> var_set_to_json
end

module Conditionals = struct
  open C_lang
  type t = Stmt.if_stmt
  let to_seq : Stmt.t -> t Seq.t =
    let f : Stmt.t -> t option =
      function
      | IfStmt i -> Some i
      | _ -> None
    in
    Stmt.find_all_map f

  let summarize (s:Stmt.t) : json =
    let count =
      to_seq s
      |> Seq.filter (fun x ->
        let open Stmt in
        Calls.has_sync x.then_stmt || Calls.has_sync x.else_stmt
      )
      |> Seq.length
    in
    `Assoc [
      "# of ifs", `Int (to_seq s |> Seq.length);
      "# of synchronized ifs", `Int count;
    ]
end


module Loops = struct
  open C_lang
  type t =
    | Do of Stmt.cond_stmt
    | While of Stmt.cond_stmt
    | For of Stmt.for_stmt

  let body : t -> Stmt.t =
    function
    | Do x -> x.body
    | While x -> x.body
    | For x -> x.body

  let is_for : t -> bool =
    function
    | For _ -> true
    | _ -> false

  let is_do : t -> bool =
    function
    | For _ -> true
    | _ -> false

  let is_while : t -> bool =
    function
    | For _ -> true
    | _ -> false

  let from_stmt : Stmt.t -> t Seq.t =
    let f : Stmt.t -> t option =
      function
      | DoStmt d -> Some (Do d)
      | WhileStmt w -> Some (While w)
      | ForStmt f -> Some (For f)
      | _ -> None
    in
    Stmt.find_all_map f

  let summarize (s:Stmt.t) : json =
    let l = from_stmt s |> List.of_seq in
    let count =
      from_stmt s
      |> Seq.filter (fun x ->
        x |> body |> Calls.has_sync
      )
      |> Seq.length
    in
    `Assoc [
      "# of for's", `Int (l |> List.filter is_for |> List.length);
      "# of while's", `Int (l |> List.filter is_while |> List.length);
      "# of do's", `Int (l |> List.filter is_do |> List.length);
      "# of synchronized loops", `Int count;
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
    | While {cond=b; _} -> "while (" ^ Expr.to_string b ^ ")"
    | Do {cond=b; _} -> "do (" ^ Expr.to_string b ^ ")"

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
    | IfStmt {then_stmt=s1; else_stmt=s2; _}
      -> to_seq s1 |> Seq.append (to_seq s2)
    | SwitchStmt {body=s; _}
    | DefaultStmt s
    | CaseStmt {body=s; _}
      -> to_seq s
    | CompoundStmt l ->
      List.to_seq l
      |> Seq.concat_map to_seq

  let infer (s: Stmt.t) : (t * Range.t option) Seq.t =
    to_seq s
    |> Seq.map (function
      | For r ->
        let o = match D_to_imp.Default.infer_range r with
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
    let missing = elems |> List.filter_map (function
      | (x, None) -> Some (`String (to_string x))
      | (_, Some _) -> None
    ) in
    `Assoc [
      "total", `Int count;
      "inferred", `Int found;
      "missing", `List missing;
    ]
end


module Accesses = struct
  open Imp

  type t = Variable.t * Access.t
  let cond_accesses (s: stmt) : t Seq.t =
    let rec cond_accesses (in_cond:bool) (s: stmt) : t Seq.t =
      match s with
      | Decl _
      | Sync
      | Assert _
      | LocationAlias _ ->
        Seq.empty
      | Acc a ->
        if in_cond then (Seq.return a) else Seq.empty
      | Block l ->
        Seq.concat_map (cond_accesses in_cond) (List.to_seq l)
      | If (_, s1, s2) ->
        Seq.append (cond_accesses true s1) (cond_accesses true s2)
      | For (_, s) ->
        cond_accesses in_cond s
    in
    cond_accesses false s

  (* Search for all loops available *)
  let all_accesses: stmt -> t Seq.t =
    let f (s:stmt) =
      match s with
      | Acc a -> Some a
      | _ -> None
    in
    find_all_map f

  let summarize (s:stmt) : json =
    let elems = all_accesses s |> List.of_seq in
    let cond_elems = cond_accesses s |> List.of_seq in
    let get_vars (x, y) = List.map fst x, List.map fst y in
    let reads, writes =
      elems
      |> List.partition (fun (_, a) -> Access.is_read a)
      |> get_vars
    in
    let c_reads, c_writes =
      cond_elems
      |> List.partition (fun (_, a) -> Access.is_read a)
      |> get_vars
    in
    `Assoc [
      "conditional reads", var_list_to_json c_reads;
      "conditional writes", var_list_to_json c_writes;
      "writes", var_list_to_json reads;
      "reads", var_list_to_json writes;
    ]
end


module Divergence = struct
  open Imp

  type t = stmt

  (* Search for all loops available *)
  let branch_with_tids: stmt -> t Seq.t =
    let f (s:stmt) =
      match s with
      | If (c, _, _) -> Freenames.contains_tid_bexp c
      | For (r, _) -> Freenames.contains_tid_range r
      | _ -> false
    in
    find_all f

  let summarize (s:stmt) : json =
    let elems = branch_with_tids s |> List.of_seq in
    let for_count =
      elems
      |> List.filter is_for
      |> List.length
    in
    let if_count =
      elems
      |> List.filter is_if
      |> List.length
    in
    `Assoc [
      "# loops with tids", `Int for_count;
      "# ifs with tids", `Int if_count;
    ]
end


module Kernel = struct
  open Imp

  let summarize (k:p_kernel) : json =
    let arrays =
      k.p_kernel_arrays
      |> Variable.Map.bindings
      |> List.map (fun ((k:Variable.t), a) ->
        let open Memory in
        `Assoc [
          "name", `String (Variable.name k);
          "hierarchy", `String (a.hierarchy |> Hierarchy.to_string);
          "size", `List (List.map (fun x -> `Int x) a.size);
          "data_type", `List (List.map (fun x -> `String x) a.data_type);
        ]
      )
    in
    `Assoc [
      "name", `String k.p_kernel_name;
      "arrays", `List arrays;
    ]
end
