open Protocols
open Stage0
(*
  Given a protocol, generates a sequence of accesses with their
  surrounding context (loops and conditionals).

  Additionally, we:
    - convert from multiple-dimension accesses to a single dimension
    - take into account the byte size of the array type
    - convert non-uniform loops into uniform loops
*)

let word_size = 4

type shared_access = {shared_array: Variable.t; index: Exp.nexp}

type t =
  | Loop of Exp.range * t
  | Cond of Exp.bexp * t
  | Index of shared_access

module Make (S:Subst.SUBST) = struct
  module M = Subst.Make(S)

  let rec subst (s:S.t) : t -> t =
    function
    | Loop (r, acc) -> Loop (M.r_subst s r, subst s acc)
    | Cond (b, acc) -> Cond (M.b_subst s b, subst s acc)
    | Index a -> Index { a with index = M.n_subst s a.index }

end

module S1 = Make(Subst.SubstPair)

let subst = S1.subst

let rec to_string : t -> string =
  function
  | Loop (r, acc) ->
      "for (" ^ Serialize.PPrint.r_to_s r ^ ") " ^ to_string acc
  | Cond (b, acc) ->
      "if ( " ^ Serialize.PPrint.b_to_s b ^ " ) " ^ to_string acc
  | Index a ->
      "[" ^ Serialize.PPrint.n_to_s a.index ^ "]"

let rec shared_array : t -> Variable.t =
  function
  | Index a -> a.shared_array
  | Loop (_, p)
  | Cond (_, p) -> shared_array p

let location (x: t) : Location.t =
  x
  |> shared_array
  |> Variable.location

type array_size = { byte_count: int; dim: int list}


(*
  Maximizes the given expression, and replaces tids by concrete values.
  *)
let maximize ?(timeout=100) (thread_count:Vec3.t) (n:Exp.nexp) : (Variable.t * Exp.nexp) list =
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
  let open Z3expr in
  let ctx = mk_context ["timeout", string_of_int timeout] in
  let n_expr = n_to_expr ctx n in
  let lb = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "?lb") in
  let restrict tid tid_count =
    let lhs = n_ge (Var tid) (Num 0) in
    let rhs = n_lt (Var tid) (Num tid_count) in
    b_to_expr ctx (b_and lhs rhs)
  in
  let opt = Optimize.mk_opt ctx in
  Optimize.add opt [
      Boolean.mk_eq ctx lb n_expr;
      restrict Variable.tidx thread_count.x;
      restrict Variable.tidy thread_count.y;
      restrict Variable.tidz thread_count.z;
    ]
  ;
  match solve opt lb (fun m ->
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
          let tid_val = Expr.to_string tid_val |> int_of_string in
          Some (tid, Num tid_val)
        with
          Failure _ -> None)
    )
  ) with
  | Some l -> l
  | None ->
      (print_endline ("ERROR: could not maximize expression: " ^ Serialize.PPrint.n_to_s n);
      [
        Variable.tidx, Num 0;
        Variable.tidy, Num 0;
        Variable.tidz, Num 0
      ])

let uniform (thread_count:Vec3.t) (r:Exp.range) : Exp.range =
  let open Exp in
  let fvs = Freenames.free_names_range r Variable.Set.empty in
  if Variable.contains_tids fvs then
    let r_subst (r:range) : (Variable.t * Exp.nexp) list -> range =
      List.fold_left (fun r (k,v) -> Subst.ReplacePair.r_subst (k, v) r) r
    in
    let r' =
      maximize thread_count (n_minus r.range_upper_bound r.range_lower_bound)
      |> r_subst r
    in
    print_endline ("Making range uniform: for (" ^ Serialize.PPrint.r_to_s r ^ ") ⇨ for (" ^ Serialize.PPrint.r_to_s r' ^ ")");
    r'
  else r

let from_kernel (thread_count:Vec3.t) (k: Proto.prog Proto.kernel) : t Seq.t =
  let open Exp in
  let shared : array_size Variable.Map.t = Variable.Map.filter_map (fun _ v ->
    if v.array_hierarchy = SharedMemory then
      let open Inference in
      let ty = Common.join " " v.array_type |> C_type.make in
      match C_type.sizeof ty with
      | Some n -> Some {byte_count=n; dim=v.array_size}
      | None -> Some {byte_count=word_size; dim=v.array_size}
    else
      None
    ) k.kernel_arrays
  in
  let rec on_i : Proto.inst -> t Seq.t =
    function
    | Proto.Acc (x, {access_index=l; _}) ->
      (* Flatten n-dimensional array and apply word size *)
      (match Variable.Map.find_opt x shared with
      | Some a ->
        (* Accumulate the values so that when we have
          [2, 2, 2] -> [1, 2, 4]
          *)
        let dim = a.dim |> List.rev in
        let dim = List.fold_left (fun (mult, l) n ->
          (n * mult, mult :: l)
        ) (1, []) dim |> snd
        in
        let e = List.fold_right (fun (n, offset) accum ->
          n_plus (n_mult (n_div n (Num word_size)) (Num (a.byte_count * offset))) accum
        ) (Common.zip l dim) (Num 0)
        in
        Seq.return (Index {shared_array=x; index=e})
      | None -> Seq.empty)
    | Proto.Sync ->
      Seq.empty
    | Proto.Cond (b, p) ->
      on_p p
      |> Seq.map (fun (i:t) : t -> Cond (b, i))
    | Proto.Loop (r, p) ->
      on_p p
      |> Seq.map (fun i -> (Loop (uniform thread_count r, i)))

  and on_p (l: Proto.prog) : t Seq.t =
    List.to_seq l |> Seq.flat_map on_i
  in
  on_p k.kernel_code
