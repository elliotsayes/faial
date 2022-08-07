module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap
type json = Yojson.Basic.t

let analyze (j:Yojson.Basic.t) : Cast.c_program  * Dlang.d_program * (Imp.p_kernel list) =
  let open Cast in
  let open D_to_imp in
  match Cast.parse_program j with
  | Ok k1 ->
    let k2 = Dlang.rewrite_program k1 in
      (match D_to_imp.parse_program k2 with
      | Ok k3 -> (k1, k2, k3)
      | Error e ->
        Cast.print_program k1;
        print_endline "------";
        Dlang.print_program k2;
        print_endline "-------";
        D_to_imp.print_error e;
        exit(-1)
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)


module Calls = struct
  type t = {func: Cast.c_exp; args: Cast.c_exp list}

  let to_seq (c:Cast.c_stmt) : t Seq.t =
    let rec to_seq (e:Cast.c_exp) : t Seq.t =
      match e with
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
    Cast.VisitStmt.to_expr_seq c
    |> Seq.concat_map to_seq

  let count (c:Cast.c_stmt) : int StringMap.t =
    to_seq c
    |> Seq.concat_map (fun c ->
      match c.func with
      | FunctionDecl x -> Seq.return (Exp.var_name x.name)
      | _ -> Seq.empty
    )
    |> Seq.fold_left (fun wc name ->
      wc |> StringMap.update name (function
        | Some n -> Some (n + 1)
        | None -> Some 1
      )
    ) StringMap.empty

  let has_sync (c:Cast.c_stmt) : bool =
    count c |> StringMap.mem "__syncthreads"

end

module Loops = struct
  open Cast
  type 'a t =
    | While of {cond: c_exp; body: 'a t}
    | For of {init: c_for_init option; cond: c_exp option; inc: c_exp option; body: 'a t}
    | Do of {cond: c_exp; body: 'a t}
    | Data of 'a

  let to_seq (c:Cast.c_stmt) : c_stmt t Seq.t =
    let rec to_seq ~in_loop:(in_loop:bool) (c:Cast.c_stmt) : c_stmt t Seq.t=
      (if in_loop then Seq.return (Data c) else Seq.empty)
      |> Seq.append (match c with
      | WhileStmt w ->
        to_seq ~in_loop:true w.body
        |> Seq.map (fun x -> While {cond=w.cond; body=x})
      | DoStmt w ->
        to_seq ~in_loop:true w.body
        |> Seq.map (fun x -> Do {cond=w.cond; body=x})
      | ForStmt w ->
        to_seq ~in_loop:true w.body
        |> Seq.map (fun x -> For {
          init=w.init;
          cond=w.cond;
          inc=w.inc;
          body=x
        })
      | BreakStmt
      | GotoStmt
      | ReturnStmt
      | ContinueStmt
      | DeclStmt _
      | SExp _ 
      -> Seq.empty
      | IfStmt s ->
        to_seq ~in_loop:false s.else_stmt
        |> Seq.append (to_seq ~in_loop:false s.then_stmt)
      | CompoundStmt l ->
        List.to_seq l
        |> Seq.concat_map (to_seq ~in_loop:false)
      | DefaultStmt s
      | SwitchStmt {body=s}
      | CaseStmt {body=s}
        -> to_seq ~in_loop:false s
      )
    in
    (* the last step is to filter out Data blocks, as they
       are loop-less *)
    let is_loop : 'a t -> bool =
      function
      | Data _ -> false
      | _ -> true
    in
    to_seq ~in_loop:false c |> Seq.filter is_loop

  let to_string (s: c_stmt t) : string =
    let open Serialize in
    let opt_exp_to_s: c_exp option -> string =
      function
      | Some c -> Cast.exp_to_s c
      | None -> ""
    in
    let rec stmt_to_s : c_stmt t -> PPrint.t list =
      function
      | Data s -> Cast.stmt_to_s s
      | For f -> [
          Line ("@for (" ^
              Cast.opt_for_init_to_s f.init ^ "; " ^
              opt_exp_to_s f.cond ^ "; " ^
              opt_exp_to_s f.inc ^ ") {"
          );
          Block(stmt_to_s f.body);
          Line ("}");
        ]
      | While {cond=b; body=s} -> [
          Line ("@while (" ^ Cast.exp_to_s b ^ ") {");
          Block (stmt_to_s s);
          Line "}"
        ]
    | Do {cond=b; body=s} -> [
        Line "}";
        Block (stmt_to_s s);
        Line ("@do (" ^ exp_to_s b ^ ") {");
      ]
    in
    stmt_to_s s |> PPrint.doc_to_string

  let rec depth: 'a t -> int =
    function
    | Data _ -> 0
    | For {body=s}
    | While {body=s}
    | Do {body=s}
      ->
      1 + depth s

  let rec get: 'a t -> 'a =
    function
    | For {body=s}
    | While {body=s}
    | Do {body=s}
      -> get s
    | Data a -> a

  let max_sync_depth (s: Cast.c_stmt) : int =
    let has_sync (s: Cast.c_stmt t) : bool =
      get s |> Calls.has_sync
    in
    to_seq s
    |> Seq.filter has_sync
    |> Seq.fold_left (fun d e -> max d (depth e)) 0

  let max_depth (s: Cast.c_stmt) : int =
    to_seq s 
    |> Seq.fold_left (fun d e -> max d (depth e)) 0
end

let main (fname: string) : unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let (k1, k2, k3) = analyze j in 
  print_endline "\n==================== STAGE 1: C\n";
  Cast.print_program ~modifier:false k1;
  print_endline "==================== STAGE 2: C with reads/writes as statements\n";
  Dlang.print_program k2;
  print_endline "==================== STAGE 3: Memory access protocols\n";
  List.iter Imp.print_kernel k3;
  print_endline "==================== STAGE 4: stats\n";
  k1 |> List.iter Cast.(function
    | Kernel k ->
      let func_count : (string * json) list = Calls.count k.code
      |> StringMap.bindings
      |> List.map (fun (k,v) -> k, `Int v)
      in
      let stats : json = `Assoc [
        "name", `String k.name;
        "function count", `Assoc func_count;
        "max loop depth", `Int (Loops.max_depth k.code);
        "max sync-loop depth", `Int (Loops.max_sync_depth k.code);
      ] in
      print_endline (Yojson.Basic.pretty_to_string stats);
    | Declaration _ -> ()
  )

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Print the C-AST" in
  Term.info "c-ast" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (main_t, info)

