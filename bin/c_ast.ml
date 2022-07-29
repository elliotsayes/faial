module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap

let analyze (j:Yojson.Basic.t) : Cast.c_program  * Dlang.d_program * (Imp.p_kernel list) =
  let open Indexflow in
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

module Call = struct
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
      let x = Call.count k.code
      |> StringMap.bindings
      |> List.map (fun (k,v) ->
          k ^ " -> " ^ string_of_int v
      )
      |> Common.join ", "
      in
      print_endline (k.name ^ " functions used: {" ^ x ^ "}")
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

