open Stage0
open Protocols
open Bc
module Vec3 = Vectorized.Vec3

(* ----------------- constants -------------------- *)

let tidx = Vectorized.tidx
let tidy = Vectorized.tidy
let tidz = Vectorized.tidz

let tid_var_list : Variable.t list = [tidx; tidy; tidz]

let tid_var_set : Variable.Set.t = Variable.Set.of_list tid_var_list

let contains_tids (vs:Variable.Set.t) : bool =
  Variable.Set.mem tidx vs ||
  Variable.Set.mem tidy vs ||
  Variable.Set.mem tidz vs

let num_banks : int = 32
let word_size = 4

(* ----------------- acc_t type -------------------- *)

module Slice = struct
  (*
    Given a protocol, generates a sequence of accesses with their
    surrounding context (loops and conditionals).

    Additionally, we:
      - convert from multiple-dimension accesses to a single dimension
      - take into account the byte size of the array type
      - convert non-uniform loops into uniform loops
  *)


  type t =
    | Loop of Exp.range * t
    | Cond of Exp.bexp * t
    | Index of Exp.nexp

  module Make (S:Subst.SUBST) = struct
    module M = Subst.Make(S)

    let rec subst (s:S.t) : t -> t =
      function
      | Loop (r, acc) -> Loop (M.r_subst s r, subst s acc)
      | Cond (b, acc) -> Cond (M.b_subst s b, subst s acc)
      | Index a -> Index (M.n_subst s a)

  end

  module S1 = Make(Subst.SubstPair)

  let subst = S1.subst

  let rec to_string : t -> string =
    function
    | Loop (r, acc) ->
        Serialize.PPrint.r_to_s r ^ ": " ^ to_string acc
    | Cond (b, acc) ->
        "if ( " ^ Serialize.PPrint.b_to_s b ^ " ) " ^ to_string acc
    | Index a ->
        "[" ^ Serialize.PPrint.n_to_s a ^ "]"

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
        restrict tidx thread_count.x;
        restrict tidx thread_count.y;
        restrict tidx thread_count.z;
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
      |> List.filter (fun (_, tid) -> Variable.Set.mem tid tid_var_set)
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
          tidx, Num 0;
          tidy, Num 0;
          tidz, Num 0
        ])

  let uniform (thread_count:Vec3.t) (r:Exp.range) : Exp.range =
    let open Exp in
    let fvs = Freenames.free_names_range r Variable.Set.empty in
    if contains_tids fvs then
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
          let dim = List.map (fun n -> a.byte_count / word_size * n) a.dim in
          let dim = List.fold_left (fun (mult, l) n ->
            (n * mult, mult :: l)
          ) (a.byte_count / word_size, []) (List.rev dim) |> snd
          in
          let e = List.fold_right (fun (n, offset) accum ->
            n_plus (n_mult n (Num (offset))) accum
          ) (Common.zip l dim) (Num 0)
          in
          Seq.return (Index e)
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
end


module IndexAnalysis = struct
  (*
    Given an arithmetic expression perform index analysis that yields the
    number of bank conflicts:
     1. remove any offsets that exist, ex `10 + tid` becomes `tid`
     2. evaluate any expression with constants and tids
    *)

  (* Given a numeric expression try to remove any offsets in the form of
     `expression + constant` or `expression - constant`.

     The way we do this is by first getting all the free-names that are
     **not** tids. Secondly, we rearrange the expression as a polynomial
     in terms of each free variable. Third, we only keep polynomials that
     mention a tid, otherwise we can safely discard such a polynomial.
     *)
  let remove_offset (fvs: Variable.Set.t) (n: Exp.nexp) : Exp.nexp =
    let rec rm_offset (n: Exp.nexp) : Variable.t list -> Exp.nexp =
      function
      | x :: fvs ->
        print_endline ("Removing offset variable '" ^ Variable.name x ^ "' from: " ^ Serialize.PPrint.n_to_s n);
        Poly.from_nexp x n
        (* We only want to keep polynomials that mention tid *)
        |> Poly.filter (fun coef _ ->
          Freenames.free_names_nexp coef Variable.Set.empty
          |> contains_tids
        )
        (* Recurse to remove any other constant factor mentioning fvs *)
        |> Poly.map (fun n _ ->
          rm_offset n fvs
        )
        (* Now convert back to a numeric expression *)
        |> Poly.to_nexp x

      | [] -> n
    in
    if Variable.Set.cardinal fvs > 0 then
      let n = rm_offset n (Variable.Set.elements fvs) in
      print_endline ("Expression without offsets: " ^ Serialize.PPrint.n_to_s n);
      n
    else n

  (*
    1. If the expressions contains any thread-local variable, return the max
       number of bank conflicts
    2. Remove any uniform offsets that appear in the expression
    3. Try to evaluate the expression, which will only work if the expression
       does _not_ contain any variables.
    4. Otherwise, return the max number of bank conflicts.
  *)
  let analyze (thread_count:Vec3.t) (thread_locals : Variable.Set.t) (n : Exp.nexp) : int =
    let bc_fail (reason : string) : int =
      Printf.eprintf
        "WARNING: %s: %s\n"
        reason (Serialize.PPrint.n_to_s n);
      num_banks
    in
    let thread_locals =
      thread_locals
      |> Variable.Set.remove tidx
      |> Variable.Set.remove tidy
      |> Variable.Set.remove tidz
    in
    let fvs = Freenames.free_names_nexp n Variable.Set.empty in
    let has_thread_locals : bool =
      not (Variable.Set.inter thread_locals fvs |> Variable.Set.is_empty)
    in
    if has_thread_locals then
      bc_fail "Expression uses thread-local variables"
    else
      let ctx =
        let open Vectorized in
          make
          ~bank_count:num_banks
          ~warp_count:num_banks
          ~use_array:(fun _ -> true)
        |> put_tids thread_count
      in
      let fvs_minus_tids = Variable.Set.diff fvs tid_var_set in
      let n = remove_offset fvs_minus_tids n in
      try
        (Vectorized.access n ctx |> Vectorized.NMap.max).value - 1
      with
        Failure _ ->
        bc_fail "Could not analyze expression"

end

module SymExp = struct
  (*
    1. Generates a summation from a slice.
    2. Flattens a summation expression as a single numeric expression.
   *)
  open Exp
  type t =
    | Const of int
    | Sum of Variable.t * Exp.nexp * t
    | Add of t list

  let rec to_string : t -> string =
    function
    | Const x -> string_of_int x
    | Sum (x, n, s) -> "Σ_{" ^ Variable.name x ^ " < " ^ Serialize.PPrint.n_to_s n ^ "} " ^ to_string s
    | Add l -> List.map to_string l |> Common.join " + "

  type factor = { power: int; divisor: int }

  let factor_to_n (e:nexp) (i: factor) : nexp =
    let rec pow (x:nexp) (n:int) : nexp =
      match n with
      | 0 -> Num 1
      | 1 -> x
      | _ -> n_mult x (pow x (n - 1))
    in
    n_div (pow e i.power) (Num i.divisor)

  let sum power e : Exp.nexp =
    let rec formula : factor list -> nexp =
      function
      | [f] -> factor_to_n e f
      | f :: l -> n_plus (factor_to_n e f) (formula l)
      | [] -> Num 0
    in
    (* https://en.wikipedia.org/wiki/Faulhaber%27s_formula *)
    match power with
    | 0 -> e
    | 1 ->
      formula [
        {power=1; divisor=2};
        {power=2; divisor=2};
      ]
    | 2 ->
      formula [
        {power=1; divisor=6};
        {power=2; divisor=2};
        {power=3; divisor=3};
      ]
    | 3 ->
      formula [
        {power=2; divisor=4};
        {power=3; divisor=2};
        {power=4; divisor=4};
      ]
    | 4 ->
      formula [
        {power=1; divisor=(-30)};
        {power=3; divisor=3};
        {power=4; divisor=2};
        {power=5; divisor=5};
      ]
    | 5 ->
      formula [
        {power=2; divisor=(-12)};
        {power=4; divisor=12};
        {power=5; divisor=2};
        {power=6; divisor=6};
      ]
    | 6 ->
      formula [
        {power=1; divisor=42};
        {power=3; divisor=(-6)};
        {power=5; divisor=2};
        {power=6; divisor=2};
        {power=7; divisor=7};
      ]
    | _ -> failwith ("S_" ^ string_of_int power ^ " not implemented")

  let rec flatten : t -> Exp.nexp =
    function
    | Const k -> Num k
    | Sum (x, ub, s) ->
      Poly.from_nexp x (flatten s)
      |> Poly.to_seq
      |> Seq.map (fun (coefficient, degree) ->
        n_mult coefficient (sum degree ub)
      )
      |> Seq.fold_left n_plus (Num 0)
    | Add l ->
      List.map flatten l
      |> List.fold_left n_plus (Num 0)

  let rec from_slice (thread_count:Vec3.t) (locs:Variable.Set.t) : Slice.t -> t =
    function
    | Index a -> Const (IndexAnalysis.analyze thread_count locs a)
    | Cond (_, p) -> from_slice thread_count locs p
    | Loop (r, p) ->
      match r with
      | {
          range_var=x;
          range_step = StepName "pow2";
          _
        } ->
        (match Predicates.r_eval_opt r with
        | Some l ->
          let l = List.map (fun i ->
            let p = Slice.subst (x, Num i) p in
            from_slice thread_count locs p
          ) l in
          Add l
        | None -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r))
      | {
          range_var=x;
          range_lower_bound=Num 0;
          range_step = Default (Num 1);
          range_upper_bound=ub;
          _
        } ->
        Sum (x, ub, from_slice thread_count locs p)
      | {range_step = Default k; _} ->
        let open Exp in
        (* x := k (x + lb) *)
        let iters = n_minus r.range_upper_bound r.range_lower_bound in
        let new_range_var = n_mult (n_plus (Var r.range_var) r.range_lower_bound) k in
        let p = Slice.subst (r.range_var, new_range_var) p in
        (*  (ub-lb)/k *)
        Sum (r.range_var, n_div iters k, from_slice thread_count locs p)
      | _ -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r)

end


(* k_cost returns the cost of a kernel *)
let cost (thread_count:Vec3.t) (k : Proto.prog Proto.kernel) : Exp.nexp =
  let subst x n p =
    Proto.PSubstPair.p_subst (Variable.from_name x, Num n) p in
  let p =
    k.kernel_code
    |> subst "blockDim.x" thread_count.x
    |> subst "blockDim.y" thread_count.y
    |> subst "blockDim.z" thread_count.z
  in
  Slice.from_kernel thread_count { k with kernel_code = p }
  |> Seq.map (fun s ->
    let s1 = SymExp.from_slice thread_count k.kernel_local_variables s in
    let s2 = SymExp.flatten s1 in
    print_endline ("   Slice: " ^ Slice.to_string s ^ "\nSymbolic: " ^ SymExp.to_string s1 ^ "\n     Exp: " ^ Serialize.PPrint.n_to_s s2 ^ "\n");
    s2
  )
  |> Seq.fold_left Exp.n_plus (Num 0)
  |> Constfold.n_opt
