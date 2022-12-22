open Stage0
open Protocols
open Bc

(* ----------------- constants -------------------- *)

let num_banks : int = 32

(* ----------------- acc_t type -------------------- *)


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
          |> Variable.contains_tids
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
    let thread_locals = Variable.Set.diff thread_locals Variable.tid_var_set in
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
      let fvs_minus_tids = Variable.Set.diff fvs Variable.tid_var_set in
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
    | Sum (x, n, s) -> "Σ_{1 ≤ " ^ Variable.name x ^ " ≤ " ^ Serialize.PPrint.n_to_s n ^ "} " ^ to_string s
    | Add l -> List.map to_string l |> Common.join " + "

  let add (l:t list) : t = Add l

  type factor = { power: int; divisor: int }

  let rec is_zero : t -> bool =
    function
    | Const 0 -> true
    | Const _ -> false
    | Sum (_, _, s) -> is_zero s
    | Add l -> List.for_all is_zero l

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

  let rec to_maxima : t -> string =
    function
    | Const k -> string_of_int k
    | Sum (x, ub, s) -> "sum(" ^ to_maxima s ^ ", " ^ Variable.name x ^ ", 1, " ^ Serialize.PPrint.n_to_s ub ^ ")"
    | Add l -> List.map to_maxima l |> Common.join " + "

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

  let cleanup_maxima_output (x:string) : string =
    let lines = String.split_on_char '\n' x in
    let max_len = List.map String.length lines
      |> List.fold_left max 0
    in
    let offsets =
      lines
      |> List.filter_map (fun line ->
        String.to_seqi line
        |> Seq.find (fun (_, a) -> a <> ' ')
        |> Option.map fst
      )
    in
    let min_offset = List.fold_left min max_len offsets in
    lines
    |> List.map (fun line ->
      Slice.from_start min_offset
      |> Slice.substring line
    )
    |> Common.join "\n"

  let run ?(exe="maxima") (x:t) : string =
    let expr = to_maxima x ^ ",simpsum;" in
    let (_, txt) =
      let cmd = Filename.quote_command exe ["--very-quiet"; "--disable-readline"] in
      Unix.open_process cmd
      |> Common.with_process_in_out (fun (ic, oc) ->
        (* Send the expression to be processed *)
        output_string oc expr;
        (* Close output to ensure it is processed *)
        close_out oc;
        (* Receive the output *)
        Common.ic_to_string ic
      )
    in
    txt |> cleanup_maxima_output

  let rec from_slice (thread_count:Vec3.t) (locs:Variable.Set.t) : Shared_access.t -> t =
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
            let p = Shared_access.subst (x, Num i) p in
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
        let new_range_var = n_mult (n_plus (Var r.range_var) (n_plus (Num 1) r.range_lower_bound)) k in
        let p = Shared_access.subst (r.range_var, new_range_var) p in
        (*  (ub-lb)/k *)
        Sum (r.range_var, n_div iters k, from_slice thread_count locs p)
      | _ -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r)

end


let cost (thread_count:Vec3.t) ?(skip_zero=true) ?(use_maxima=true) ?(explain=true) (k : Proto.prog Proto.kernel) : string =
  let subst x n p =
    Proto.PSubstPair.p_subst (Variable.from_name x, Num n) p in
  let p =
    k.kernel_code
    |> subst "blockDim.x" thread_count.x
    |> subst "blockDim.y" thread_count.y
    |> subst "blockDim.z" thread_count.z
  in
  let handle_slice =
    if explain then
      Seq.filter_map (fun s ->
        (* Convert a slice into an expression *)
(*         Tui.LocationUI.print s.location; *)
        let s1 = SymExp.from_slice thread_count k.kernel_local_variables s in
        if skip_zero && SymExp.is_zero s1 then
          None
        else Some (
          (* Flatten the expression *)
          let simplified_cost =
            if use_maxima then
              SymExp.run s1
            else
              s1 |> SymExp.flatten |> Serialize.PPrint.n_to_s
          in
          let blue = PrintBox.Style.(set_bold true (set_fg_color Blue default)) in
          PrintBox.(
            tree (s |> Shared_access.to_string |> String.cat "▶ Context: " |> text)
            [
              tree ("▶ Cost: "  ^ SymExp.to_string s1 |> text)
              [
                tree ("▶ Cost (simplified):" |> text_with_style blue)
                [
                  text_with_style blue simplified_cost |> hpad 1
                ]
              ]
            ]
          ) |> PrintBox_text.output stdout;
          print_endline "\n";
          s1
        )
      )
    else
      Seq.map (SymExp.from_slice thread_count k.kernel_local_variables)
  in
  (* 1. break a kernel into slices *)
  let total = Shared_access.from_kernel thread_count { k with kernel_code = p }
    |> handle_slice
    |> List.of_seq
    |> SymExp.add
  in
  let total =
    if use_maxima then
      SymExp.run total
    else
      SymExp.flatten total
      |> Constfold.n_opt
      |> Serialize.PPrint.n_to_s
  in
  PrintBox.(
    text total
    |> hpad 1
    |> frame
  )
  |> PrintBox_text.to_string
