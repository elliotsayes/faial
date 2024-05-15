open Stage0
open Protocols

module Make (L:Logger.Logger) = struct
  (*
    Maximizes the given expression, and replaces tids by concrete values.
    *)
  let maximize ?(timeout=100) (block_dim:Dim3.t) (n:Exp.nexp) : (Variable.t * Exp.nexp) list =
    let open Exp in
    let solve
      (opt:Z3.Optimize.optimize)
      (lb:Z3.Expr.expr)
      (handler:Z3.Model.model -> (Variable.t * Exp.nexp) list)
    :
      (Variable.t * Exp.nexp) list option
    =
      let open Z3 in
      let _ = Optimize.maximize opt lb in
      if Optimize.check opt = Solver.SATISFIABLE then
        Optimize.get_model opt
        |> Option.map handler
      else
        None
    in
    let open Z3 in
    let ctx = mk_context ["timeout", string_of_int timeout] in
    let b_to_expr = Gen_z3.Bv32Gen.b_to_expr ctx in
    let n_to_expr = Gen_z3.Bv32Gen.n_to_expr ctx in
    let parse_num = Gen_z3.Bv32Gen.parse_num in
    let x = Var (Variable.from_name "?max") in
    let restrict tid tid_count =
      let lhs = n_ge (Var tid) (Num 0) in
      let rhs = n_lt (Var tid) (Num tid_count) in
      b_to_expr (b_and lhs rhs)
    in
    let opt = Optimize.mk_opt ctx in
    Optimize.add opt [
        (*
          Bit-vector maximization has no notion of signedness.
          The following constrain guarantees that the goal being maximized
          is a signed-positive number.

          https://stackoverflow.com/questions/64484347/
        *)
        b_to_expr (n_ge x (Num 0));
        restrict Variable.tid_x block_dim.x;
        restrict Variable.tid_y block_dim.y;
        restrict Variable.tid_z block_dim.z;
      ]
    ;
    match solve opt (n_to_expr x) (fun m ->
      (* Go through all declarations of the model *)
      Model.get_const_decls m
      |> List.map (fun d ->
        (* Convert the declaration to a variable *)
        let tid : Variable.t =
          d
          |> FuncDecl.get_name
          |> Symbol.get_string
          |> Variable.from_name
        in
        (d, tid)
      )
      (* Only keep tids *)
      |> List.filter (fun (_, tid) -> Variable.is_tid tid)
      (* Evaluate the value *)
      |> List.filter_map (fun (d, tid) ->
        (* Replace each tid by the value in the model *)
        (* Variables in the model are actually functions with
          0 args, so we create a function call *)
        (* We then evaluate the function call *)
        Model.eval m (FuncDecl.apply d []) true
        |> Option.map (fun tid_val -> (tid, tid_val))
      )
      |> List.filter_map (fun (tid, tid_val) ->
          (* Try to cast tid to an integer and then substitute *)
          (* Try to cast a value to a string, if we fail, return None *)
          (try
            let tid_val : int = Expr.to_string tid_val |> parse_num |> int_of_string in
            Some (tid, Num tid_val)
          with
            Failure _ -> None)
      )
    ) with
    | Some l -> l
    | None ->
        (L.error ("could not maximize expression: " ^ Exp.n_to_string n);
        [
          Variable.tid_x, Num 0;
          Variable.tid_y, Num 0;
          Variable.tid_z, Num 0
        ])
  (*
   Given a range, makes that range uniform according to tids.
   *)
  let uniform (block_dim:Dim3.t) (r:Range.t) : Range.t option =
    let open Exp in
    let fvs = Freenames.free_names_range r Variable.Set.empty in
    if Variable.contains_tids fvs then
      Some (
        let r_subst (r:Range.t) : (Variable.t * Exp.nexp) list -> Range.t =
          List.fold_left (fun r (k,v) -> Subst.ReplacePair.r_subst (k, v) r) r
        in
        let r' =
          maximize block_dim (n_minus r.upper_bound r.lower_bound)
          |> r_subst r
        in
        L.info ("Making range uniform: for (" ^ Range.to_string r ^ ") 🡆 for (" ^ Range.to_string r' ^ ")");
        r'
      )
    else None
end

module Silent = Make(Logger.Silent)
module Default = Make(Logger.Colors)
