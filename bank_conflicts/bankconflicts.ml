open Stage0
open Protocols

(* ----------------- constants -------------------- *)

let tidx = Variable.from_name "threadIdx.x"
let tidy = Variable.from_name "threadIdx.y"
let tidz = Variable.from_name "threadIdx.z"

let tid_var_list : Variable.t list = [tidx; tidy; tidz]

let tid_var_set : Variable.Set.t = Variable.Set.of_list tid_var_list

let contains_tids (vs:Variable.Set.t) : bool =
  Variable.Set.mem tidx vs &&
  Variable.Set.mem tidy vs &&
  Variable.Set.mem tidz vs

let num_banks : int = 32

let bc_degrees = [1; 2; 4; 8; 16; 32]
(* TODO: generate bc_degrees from num_banks *)


module Vec3 = struct
  type t = {x : int; y: int; z: int;}
  let mk ~x:x ~y:y ~z:z : t = {x=x; y=y; z=z}
  let to_string (v:t) =
    "{.x = " ^ string_of_int v.x ^
    ", .y= " ^ string_of_int v.y ^
    ", .z= " ^ string_of_int v.z ^ "}"
end

(* ----------------- acc_t type -------------------- *)

module Slice = struct

  type t =
    | Loop of Exp.range * t
    | Cond of Exp.bexp * t
    | Acc of Exp.access

  module Make (S:Subst.SUBST) = struct
    module M = Subst.Make(S)

    let rec subst (s:S.t) : t -> t =
      function
      | Loop (r, acc) -> Loop (M.r_subst s r, subst s acc)
      | Cond (b, acc) -> Cond (M.b_subst s b, subst s acc)
      | Acc a -> Acc (M.a_subst s a)

  end

  module S1 = Make(Subst.SubstPair)

  let subst = S1.subst

  let rec to_string (v : Variable.t) : t -> string =
    function
    | Loop (r, acc) ->
        Serialize.PPrint.r_to_s r ^ ": " ^ to_string v acc
    | Cond (b, acc) ->
        "if ( " ^ Serialize.PPrint.b_to_s b ^ " ) " ^ to_string v acc
    | Acc a ->
        Serialize.PPrint.doc_to_string (Serialize.PPrint.acc_expr_to_s (v, a))

  let rec access : t -> Exp.access =
    function
    | Loop (_, acc)
    | Cond (_, acc) -> access acc
    | Acc a -> a

  let from_proto (x:Variable.t) : Proto.prog -> t Seq.t =
    let rec on_i : Proto.inst -> t Seq.t =
      function
      | Proto.Acc (y, e) when Variable.equal x y ->
        Seq.return (Acc e)
      | Proto.Acc _
      | Proto.Sync ->
        Seq.empty
      | Proto.Cond (b, p) ->
        on_p p
        |> Seq.map (fun i -> Cond (b, i))
      | Proto.Loop (r, p) ->
        on_p p
        |> Seq.map (fun i -> (Loop (r, i)))

    and on_p (l: Proto.prog) : t Seq.t =
      l |> List.to_seq |> Seq.flat_map on_i
    in
    on_p

end

(* ----------------- poly_t type -------------------- *)
module Poly = struct
  open Exp
  type poly_ht = (int, nexp) Hashtbl.t

  type t =
    | One of nexp
    | Two of {constant: nexp; coeficient: nexp}
    | Many of poly_ht

  let to_string x (p:t) =
    let open Serialize in
    let open PPrint in
    match p with
    | One n -> n_to_s n
    | Two {constant=n1; coeficient=n2} -> n_par n1 ^ " + " ^ n_par n2 ^ " * " ^ x
    | Many ht ->
      Common.hashtbl_elements ht
      |> List.map (fun (k, v) -> n_par v ^ " * " ^ x ^ "^" ^ (string_of_int k))
      |> Common.join " + "

  let max_coeficient : t -> int =
    function
    | One _ -> 0
    | Two _ -> 1
    | Many ht -> Hashtbl.to_seq_keys ht |> Seq.fold_left max 0

  let make (e:nexp) (n:int) : t =
    if n = 0 then
      One e
    else if n = 1 then
      Two {constant=Num 0; coeficient=e}
    else
      let ht = Hashtbl.create 1 in
      Hashtbl.add ht n e;
      Many ht

  let update_ht (ht:('a, 'b) Hashtbl.t) (k:'a)  (f:'b option -> 'b)  : unit =
    Hashtbl.replace ht k (f (Hashtbl.find_opt ht k))

  let poly_update_ht (ht:poly_ht) (k:int) (f:nexp -> nexp) : unit =
    update_ht ht k (function | Some v -> f v | None -> f (Num 0))

  let poly_add_ht (src:poly_ht) (dst:poly_ht) : unit =
    Hashtbl.iter (fun i n ->
      poly_update_ht dst i (n_plus n)
    ) src

  let add (e1:t) (e2:t) : t =
    match e1, e2 with
    | One n1, One n2 -> One (n_plus n1 n2)
    | One n1, Two {constant=n2; coeficient=n3}
    | Two {constant=n2; coeficient=n3}, One n1 ->
      Two {constant=n_plus n2 n1; coeficient=n3}
    | Two {constant=n1; coeficient=n2}, Two {constant=n3; coeficient=n4} ->
      Two {constant=n_plus n1 n3; coeficient=n_plus n2 n4}
    | One n1, Many ht
    | Many ht, One n1 ->
      let ht = Hashtbl.copy ht in
      poly_update_ht ht 0 (n_plus n1);
      Many ht
    | Two {constant=n1; coeficient=n2}, Many ht
    | Many ht, Two {constant=n1; coeficient=n2} ->
      let ht = Hashtbl.copy ht in
      poly_update_ht ht 0 (n_plus n1);
      poly_update_ht ht 1 (n_plus n2);
      Many ht
    | Many ht1, Many ht2 ->
      let ht2 = Hashtbl.copy ht2 in
      poly_add_ht ht1 ht2;
      Many ht2

  let rec mult (e1:t) (e2:t) : t =
    let mult_ht (src:poly_ht) ((i1,n1):int*nexp) : poly_ht =
      (* z * x * (a + b*x + c*x^2) = a * z * x + z * b * x ^ 2 ... *)
      let dst = Hashtbl.create (Hashtbl.length src) in
      Hashtbl.iter (fun i2 n2 ->
        Hashtbl.add dst (i1 + i2) (n_mult n1 n2)
      ) src;
      dst
    in
    let mk_poly_ht (n1:nexp) (n2:nexp) : poly_ht =
      let ht = Hashtbl.create 2 in
      Hashtbl.add ht 0 n1;
      Hashtbl.add ht 1 n2;
      ht
    in
    match e1, e2 with
    | One n1, One n2 ->
      One (n_mult n1 n2)

    | One n1, Two {constant=n2; coeficient=n3}
    | Two {constant=n2; coeficient=n3}, One n1 ->
      Two {constant=n_mult n1 n2; coeficient=n_mult n1 n3}

    | Two {constant=n1; coeficient=n2}, Two {constant=n3; coeficient=n4} ->
        let ht' = mult_ht (mk_poly_ht n3 n4) (1, n2) in
        add (mult (One n1) e2) (Many ht')

    | One n1, Many ht
    | Many ht, One n1 ->
      Common.hashtbl_elements ht
      |> List.map (fun (i, n) -> (i, n_mult n n1))
      |> Common.hashtbl_from_list
      |> (fun ht -> Many ht)

    | Two {constant=n1; coeficient=n2}, Many ht
    | Many ht, Two {constant=n1; coeficient=n2}
      -> mult (Many (mk_poly_ht n1 n2)) (Many ht)
    | Many ht1, Many ht2 ->
      let ht = Hashtbl.create ((Hashtbl.length ht1) * (Hashtbl.length ht2)) in
      Common.hashtbl_elements ht1
      |> List.map (mult_ht ht2)
      |> List.iter (fun src ->
        poly_add_ht src ht
      );
      Many ht

  let uminus (p:t) : t =
    let u_minus n = n_mult (Num (-1)) n in
    match p with
    | One n -> One (u_minus n)
    | Two {constant=n1; coeficient=n2} ->
      Two {constant=u_minus n1; coeficient=u_minus n2}

    | Many ht -> Common.hashtbl_elements ht
      |> List.map (fun (k, v)-> (k, u_minus v))
      |> fun l -> Many (Common.hashtbl_from_list l)

  let to_seq : t -> (nexp*int) Seq.t =
    function
    | One x -> Seq.return (x, 0)
    | Two {constant=n1; coeficient=n2} -> [n1, 0; n2, 1] |> List.to_seq
    | Many ht ->
      Common.hashtbl_elements ht
      |> List.to_seq
      |> Seq.map (fun (x, y) -> (y, x))

  let rec from_nexp v (n:nexp) : t =
    match n with
    | Var x -> if Variable.equal x v then Two {constant=Num 0; coeficient=Num 1} else One n
    | Num _ -> One n
    | Proj _
    | NCall _
    | NIf _ -> One (Num 0)
    | Bin (Plus, e1, e2) -> add (from_nexp v e1) (from_nexp v e2)
    | Bin (Minus, e1, e2) -> add (from_nexp v e1) (uminus (from_nexp v e2))
    | Bin (Mult, e1, e2) -> mult (from_nexp v e1) (from_nexp v e2)
    | Bin _ -> One (Num 0)


end

(* ----------------- transaction cost analysis -------------------- *)

(* This function indicates whether our theory CAN analyze the expression, not
   if there are bank-conflicts!  Returns None if we CANNOT analyze. *)
let handle_bank_conflicts (n:Exp.nexp) : Poly.t option =
  let handle_coefficient (n:Exp.nexp) : bool =
    let fns = Freenames.free_names_nexp n Variable.Set.empty in
    Variable.Set.disjoint tid_var_set fns
  in
  let handle_poly (x: Variable.t) : Poly.t option =
    let p = Poly.from_nexp x n in
    match p with
    | One n ->
      (* var x (e.g., threadIdx.x) is not in the expression *)
      if handle_coefficient n then Some p else None
    | Two {constant=c; coeficient=k} ->
      (* The expression is of form: (k * x + c) *)
      if handle_coefficient c && handle_coefficient k
      then Some p else None
    | Many _ -> None
  in List.find_map handle_poly tid_var_list
(*
  Either maximizes or minimizes n to replace tids by constants.
  *)
let solve f (thread_count:Vec3.t) (n:Exp.nexp) : Exp.nexp =
  let open Exp in
  let solve
    (opt:Z3.Optimize.optimize)
    (lb:Z3.Expr.expr)
    (handler:Z3.Model.model -> Exp.nexp)
  :
    Exp.nexp
  =
    let open Z3 in
    let _ = f opt lb in
    if Optimize.check opt = Solver.SATISFIABLE then
      Optimize.get_model opt
      |> Option.map handler
      |> fun x -> Option.value x ~default:n
    else
      n
  in
  let fvs = Freenames.free_names_nexp n Variable.Set.empty in
  if contains_tids fvs then begin
    let open Z3 in
    let open Z3expr in
    let ctx = mk_context [] in
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
    solve opt lb (fun m ->
      (* Go through all declarations of the model *)
      List.fold_left (fun accum d ->
        (* For each declaration, if it's one of the TIDs, then
          replace tid by the value in the model in the accumulator
          *)
        (* Convert the declaration to a variable *)
        let tid : Variable.t =
          d
          |> FuncDecl.get_name
          |> Symbol.get_string
          |> Variable.from_name
        in
        (* If the variable is a tid *)
        if Variable.Set.mem tid tid_var_set then (
          (* Replace each tid by the value in the model *)
          (* Variables in the model are actually functions with
            0 args, so we create a function call *)
          let e = FuncDecl.apply d [] in
          (* We then evaluate the function call *)
          match Model.eval m e true with
          | Some tid_val ->
            (* Try to cast tid to an integer and then substitute *)
            (* Try to cast a value to a string, if we fail, return None *)
            (try
              let tid_val = Expr.to_string tid_val |> int_of_string in
              Subst.ReplacePair.n_subst (tid, Num tid_val) accum
            with
              Failure _ -> accum)
          | None -> accum
        ) else accum
      ) n (Model.get_const_decls m)
    )
  end else n

let minimize = solve Z3.Optimize.minimize
let maximize = solve Z3.Optimize.maximize


(* p_cost returns bank conflict degree of a poly p *)
let p_cost : Poly.t -> int = function
  (* constant access pattern: this is a broadcast *)
  | One _ ->
    1
  (* linear access pattern: maximize degree with Z3 *)
  | Two {constant=_; coeficient=n} ->
    (* we call Z3 from here to calculate the bank conflict degree *)
    let open Z3 in
    let open Z3expr in
    (* print_endline (Freenames.free_names_nexp n); *)
    let ctx = mk_context [] in
    let n_expr = n_to_expr ctx n in
    let k = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "?k") in
    let d = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "?d") in
    let num (n : int) = Arithmetic.Integer.mk_numeral_i ctx n in
    let k_eq = Boolean.mk_eq ctx k n_expr in
    let d_eq = Boolean.mk_or ctx (bc_degrees |> List.map (fun n ->
      Boolean.mk_eq ctx d (num n))) in
    let d_divides_k = Boolean.mk_eq ctx
      (Arithmetic.Integer.mk_mod ctx k d) (num 0) in
    let opt = Optimize.mk_opt ctx in
    Optimize.add opt [ k_eq; d_eq; d_divides_k ];
    let handle = Optimize.maximize opt d in
    (* print_string (Optimize.to_string opt); *) (* print SMT-LIB *)
    assert (Optimize.check opt = Solver.SATISFIABLE);
    Z.to_int (Arithmetic.Integer.get_big_int (Optimize.get_upper handle))
  (* non-linear access pattern: theory incomplete *)
  | Many _ -> num_banks

let has_thread_locals (locs : Variable.Set.t) (n:Exp.nexp) : bool =
  let fvs = Freenames.free_names_nexp n Variable.Set.empty in
  not (Variable.Set.inter locs fvs |> Variable.Set.is_empty)
(* https://cs.calvin.edu/courses/cs/374/CUDA/CUDA-Thread-Indexing-Cheatsheet.pdf *)
(* n_cost returns bank conflict degree of a poly n *)
let n_cost (locs : Variable.Set.t) (n : Exp.nexp) : int =
  let bc_fail (reason : string) : int =
    Printf.eprintf
      "WARNING: %s: %s: assuming worst case bank conflict of %d\n"
      reason (Serialize.PPrint.n_to_s n) num_banks;
    num_banks
  in
  let locs = Variable.Set.remove (Variable.from_name "threadIdx.x") locs in
  if has_thread_locals locs n then num_banks
  else
    let ctx = Vectorized.make
      ~bank_count:32 ~tid_count:32 ~use_array:(fun _ -> true)
    in
      try
        Vectorized.access n ctx |> Vectorized.NMap.max |> snd
      with
        Failure _ -> (
        match handle_bank_conflicts n with
        | Some p ->
          begin try p_cost p with
          | Z3expr.Not_implemented e -> bc_fail e (* Z3expr TODO *)
          end
        | None -> bc_fail "pattern not linear"    (* theory TODO *)
        )

(* access_cost returns bank conflict cost of an access *)
let access_cost (locs:Variable.Set.t) (a : Exp.access) : int =
  List.fold_left (+) 0 (List.map (n_cost locs) a.access_index)


(* ----------------- kernel cost analysis -------------------- *)


module SymExp = struct
  open Exp
  type t =
    | Const of int
    | Sum of Variable.t * Exp.nexp * t

  let rec to_string : t -> string =
    function
    | Const x -> string_of_int x
    | Sum (x, n, s) -> "Σ_{" ^ Variable.name x ^ " < " ^ Serialize.PPrint.n_to_s n ^ "} " ^ to_string s

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
end


let rec slice_to_sym (thread_count:Vec3.t) (locs:Variable.Set.t) : Slice.t -> SymExp.t =
  function
  | Acc a -> Const (access_cost locs a)
  | Cond (_, p) -> slice_to_sym thread_count locs p
  | Loop (r, p) ->
    match r with
    | {
        range_var=x;
        range_lower_bound=Num 0;
        range_step = Default (Num 1);
        range_upper_bound=ub;
        _
      } ->
      Sum (x, ub, slice_to_sym thread_count locs p)
    | {range_step = Default (Num 1); _} ->
      let open Exp in
      let lb = r.range_lower_bound |> minimize thread_count in
      let ub = r.range_upper_bound |> maximize thread_count in
      (* subst [range_var := range_var + lower_bound]: *)
      let new_range_var = n_plus (Var r.range_var) lb in
      let p = Slice.subst (r.range_var, new_range_var) p in
      (* rewrite lower_bound..upper_bound to 0..(upper_bound-lower_bound): *)
      let upper_bound = n_minus ub lb in
      Sum (r.range_var, upper_bound, slice_to_sym thread_count locs p)
    | {range_step = Default k; _} ->
      let open Exp in
      let lb = r.range_lower_bound |> minimize thread_count in
      let ub = r.range_upper_bound |> maximize thread_count in
      (* x := k (x + lb) *)
      let new_range_var = n_mult (n_plus (Var r.range_var) lb) k in
      let p = Slice.subst (r.range_var, new_range_var) p in
      let iters = n_minus ub lb in
      (*  (ub-lb)/k *)
      let upper_bound = n_div iters k in
      Sum (r.range_var, upper_bound, slice_to_sym thread_count locs p)
    | _ -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r)

(* acc_t_cost returns cost of an acc_t expression *)
let slice_to_nexp (thread_count:Vec3.t) (locs:Variable.Set.t) (acc : Slice.t) : Exp.nexp =
  acc
  |> slice_to_sym thread_count locs
  |> (fun x -> print_endline (SymExp.to_string x); x)
  |> SymExp.flatten


(* shared_cost returns the cost of all accesses to a shared memory array *)
let shared_cost (thread_count:Vec3.t) (k : Proto.prog Proto.kernel) (v : Variable.t) : Exp.nexp Seq.t =
  Slice.from_proto v k.kernel_code
  |> Seq.map (slice_to_nexp thread_count k.kernel_local_variables)

(* k_cost returns the cost of a kernel *)
let k_cost (thread_count:Vec3.t) (k : Proto.prog Proto.kernel) : Exp.nexp Seq.t =
  Proto.kernel_shared_arrays k
  |> Variable.Set.to_seq
  |> Seq.concat_map (shared_cost thread_count k)

(* p_k_cost returns the cost of all kernels in the program source *)
let p_k_cost (thread_count:Vec3.t) (ks : Proto.prog Proto.kernel list) : Exp.nexp =
  List.to_seq ks
  |> Seq.concat_map (k_cost thread_count)
  |> Seq.fold_left Exp.n_plus (Num 0)
  |> Constfold.n_opt
