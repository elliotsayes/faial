open Stage0
open Stage1
open Inference

module StringMap = Common.StringMap
module VarSet = Variable.Set
module VarMap = Variable.Map
type json = Yojson.Basic.t

let analyze (j:Yojson.Basic.t) : C_lang.c_program  * D_lang.d_program * (Imp.p_kernel list) =
  let open C_lang in
  let open D_to_imp in
  match C_lang.parse_program j with
  | Ok k1 ->
    let k2 = D_lang.rewrite_program k1 in
      (match D_to_imp.parse_program k2 with
      | Ok k3 -> (k1, k2, k3)
      | Error e ->
        C_lang.print_program k1;
        print_endline "------";
        D_lang.print_program k2;
        print_endline "-------";
        D_to_imp.print_error e;
        exit(-1)
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)


module Calls = struct
  type t = {func: C_lang.Expr.t; args: C_lang.Expr.t list}

  (* Returns all function calls in a statement, as
        a sequence. *)
  let to_seq (c:C_lang.c_stmt) : t Seq.t =
    (** Returns all function calls in an expression, as
        a sequence. *)
    let rec to_seq (e:C_lang.Expr.t) : t Seq.t =
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
    C_lang.VisitStmt.to_expr_seq c
    |> Seq.concat_map to_seq

  let count (c:C_lang.c_stmt) : int StringMap.t =
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
  let has_sync (c:C_lang.c_stmt) : bool =
    count c |> StringMap.mem "__syncthreads"

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
        data: c_stmt;
      }
    | For of {
        init: ForInit.t option;
        cond: Expr.t option;
        inc: Expr.t option;
        body: loop list;
        data: c_stmt;
      }
    | Do of {
        cond: Expr.t;
        body: loop list;
        data: c_stmt;
      }

  type t = loop list

  let make : C_lang.c_stmt -> t =
    let rec to_seq (c:C_lang.c_stmt) : t =
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

  let data : loop -> c_stmt =
    function
    | For {data=s}
    | While {data=s}
    | Do {data=s}
      -> s

  (* Given set of loops, filters out unsynchronized loops. *)
  let filter_sync : t -> t =
    filter (fun l -> data l |> Calls.has_sync )

  let summarize (s:c_stmt) : json =
    let l = make s in
    `Assoc [
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

  let typecheck (s: c_stmt) : VarSet.t =
    let rec typecheck (scope:int) (env:int VarMap.t) : c_stmt -> int VarMap.t * VarSet.t =
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
        | Some (ForDecl l) ->
          VisitStmt.to_expr_seq (DeclStmt l)
          |> Seq.fold_left (fun vars e ->
              VarSet.union vars (typecheck_e ~scope e)
          ) VarSet.empty 
        | Some (ForExpr e) -> typecheck_e ~scope:scope e
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

  let summarize (s: c_stmt) : json =
    typecheck s
    |> var_set_to_json
end


module IntVars = struct
  open C_lang
  let rec find_vars (s:c_stmt) (vars:VarSet.t) : VarSet.t =
    let add_decls (decls:Decl.t list) : VarSet.t =
      decls
      |> List.map Decl.name
      |> VarSet.of_list
      |> VarSet.union vars
    in
    match s with
    | DeclStmt l ->
      add_decls l

    | ForStmt {init=i; body=s} ->
      let decls = match i with
      | Some (ForDecl l) -> l 
      | _ -> []
      in
      add_decls decls

    | SExpr _
    | BreakStmt
    | GotoStmt
    | ReturnStmt
    | ContinueStmt
    -> vars

    | IfStmt s ->
      vars
      |> find_vars s.then_stmt
      |> find_vars s.else_stmt

    | CompoundStmt l ->
      List.fold_left (fun vars s ->
        find_vars s vars
      ) vars l

    | DoStmt {body=s}
    | WhileStmt {body=s}
    | DefaultStmt s
    | SwitchStmt {body=s}
    | CaseStmt {body=s}
      -> find_vars s vars

  let summarize (s: c_stmt) : json =
    let n =
      find_vars s VarSet.empty
      |> VarSet.cardinal
    in
    `Int n
end


module ForEach = struct
  (* Loop inference *)
  open D_lang

  type t =
    | For of d_for
    | While of {cond: Expr.t; body: d_stmt}
    | Do of {cond: Expr.t; body: d_stmt}

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
  let rec to_seq: d_stmt -> t Seq.t =
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

  let infer (s: d_stmt) : (t * Exp.range option) Seq.t =
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

  let summarize (s:d_stmt) : json =
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

let main (fname: string) (silent:bool) : unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let (k1, k2, k3) = analyze j in
  if silent then () else ( 
    print_endline "\n==================== STAGE 1: C\n";
    C_lang.print_program ~modifier:false k1;
    print_endline "==================== STAGE 2: C with reads/writes as statements\n";
    D_lang.print_program k2;
    print_endline "==================== STAGE 3: Memory access protocols\n";
    List.iter Imp.print_kernel k3;
    print_endline "==================== STAGE 4: stats\n";
  );
  let l = k1 |> Common.map_opt C_lang.(function
    | Kernel k ->
      let func_count : (string * json) list = Calls.count k.code
      |> StringMap.bindings
      |> List.map (fun (k,v) -> k, `Int v)
      in
      let k2 = D_lang.rewrite_kernel k in
      Some (`Assoc [
        "name", `String k.name;
        "function count", `Assoc func_count;
        "loops", Loops.summarize k.code;
        "loop inference", ForEach.summarize k2.code;
        "mutated vars", MutatedVar.summarize k.code;
        "var count", IntVars.summarize k.code;
      ])
    | Declaration _ -> None
  )
  in
  print_endline (Yojson.Basic.pretty_to_string (`List l));

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let silent =
  let doc = "Silence output" in
  Arg.(value & flag & info ["silent"] ~doc)

let main_t = Term.(const main $ get_fname $ silent)

let info =
  let doc = "Print the C-AST" in
  Cmd.info "c-ast" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

