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
  | Loop of Range.t * t
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
      "for (" ^ Range.to_string r ^ ") " ^ to_string acc
  | Cond (b, acc) ->
      "if (" ^ Exp.b_to_string b ^ ") " ^ to_string acc
  | Index a ->
      "acc(" ^ Exp.n_to_string a.index ^ ")"

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
  let ctx = mk_context ["timeout", string_of_int timeout] in
  let n_expr = Gen_z3.IntGen.n_to_expr ctx n in
  let lb = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "?lb") in
  let restrict tid tid_count =
    let lhs = n_ge (Var tid) (Num 0) in
    let rhs = n_lt (Var tid) (Num tid_count) in
    Gen_z3.IntGen.b_to_expr ctx (b_and lhs rhs)
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
      (print_endline ("ERROR: could not maximize expression: " ^ Exp.n_to_string n);
      [
        Variable.tidx, Num 0;
        Variable.tidy, Num 0;
        Variable.tidz, Num 0
      ])

let uniform (thread_count:Vec3.t) (r:Range.t) : Range.t option =
  let open Exp in
  let fvs = Freenames.free_names_range r Variable.Set.empty in
  if Variable.contains_tids fvs then
    Some (
      let r_subst (r:Range.t) : (Variable.t * Exp.nexp) list -> Range.t =
        List.fold_left (fun r (k,v) -> Subst.ReplacePair.r_subst (k, v) r) r
      in
      let r' =
        maximize thread_count (n_minus r.upper_bound r.lower_bound)
        |> r_subst r
      in
      print_endline ("Making range uniform: for (" ^ Range.to_string r ^ ") 🡆 for (" ^ Range.to_string r' ^ ")");
      r'
    )
  else None

let byte_count_multiplier (byte_count:int) (l:Exp.nexp list) : Exp.nexp list =
  if byte_count/word_size = 1 then
    l
  else (
    let open Exp in
    let n_s = Exp.n_to_string in
    let bs = string_of_int byte_count ^  "/" ^ string_of_int word_size in
    let arr l = List.map n_s l |> Common.join ", " in
    let l' = List.map (fun n ->
      n_mult (Num byte_count) (n_div n (Num word_size))
      ) l
    in
    print_endline ("Applied byte-modifier : " ^ bs ^ " " ^ arr l  ^ " -> " ^ arr l');
    l'
  )

let flatten_multi_dim (dim:int list) (l:Exp.nexp list) : Exp.nexp =
  match l with
  | [e] -> e
  | _ ->
    let open Exp in
    (* Accumulate the values so that when we have
      [2, 2, 2] -> [1, 2, 4]
      *)
    let dim =
      dim
      |> List.rev
      |> List.fold_left (fun (mult, l) n ->
        (n * mult, mult :: l)
      ) (1, [])
      |> snd
    in
    List.fold_right (fun (n, offset) accum ->
      n_plus (n_mult n (Num offset)) accum
    ) (Common.zip l dim) (Num 0)

let from_kernel (thread_count:Vec3.t) (k: Proto.prog Proto.kernel) : t Seq.t =
  let open Exp in
  let shared : array_size Variable.Map.t = Variable.Map.filter_map (fun _ v ->
    if Memory.is_shared v then
      let open Inference in
      let ty = Common.join " " v.data_type |> C_type.make in
      match C_type.sizeof ty with
      | Some n -> Some {byte_count=n; dim=v.size}
      | None -> Some {byte_count=word_size; dim=v.size}
    else
      None
    ) k.kernel_arrays
  in
  let rec on_i : Proto.inst -> t Seq.t =
    function
    | Proto.Acc (x, {index=l; _}) ->
      (* Flatten n-dimensional array and apply word size *)
      (match Variable.Map.find_opt x shared with
      | Some a ->
        let e =
          l
          |> byte_count_multiplier a.byte_count
          |> flatten_multi_dim a.dim
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
      |> Seq.map (fun i ->
        match uniform thread_count r with
        | Some r' ->
          let cnd =
            b_and
              (n_ge (Var r.var) r.lower_bound)
              (n_lt (Var r.var) r.upper_bound)
          in
          Loop (r', Cond(cnd, i))
        | None ->
          Loop (r, i)
      )

  and on_p (l: Proto.prog) : t Seq.t =
    List.to_seq l |> Seq.flat_map on_i
  in
  on_p k.kernel_code
