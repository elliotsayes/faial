open Stage0
open Protocols

open Exp
open Proto
open Common


(* ----------------- constants -------------------- *)

let tid_vars : Variable.t list =
  List.map Variable.from_name
  ["threadIdx.x"; "threadIdx.y"; "threadIdx.z"]

let num_banks : int = 32

let bc_degrees = [1; 2; 4; 8; 16; 32]
(* TODO: generate bc_degrees from num_banks *)


(* ----------------- acc_t type -------------------- *)

type 'a acc_t =
  | Range of range * 'a acc_t
  | Cond of bexp * 'a acc_t
  | Acc of 'a

module Make (S:Subst.SUBST) = struct
  module M = Subst.Make(S)

  let rec acc_t_subst (s:S.t) (acc: 'a acc_t) : 'a acc_t =
    match acc with
    | Range (r, acc) -> Range (M.r_subst s r, acc_t_subst s acc)
    | Cond (b, acc) -> Cond (M.b_subst s b, acc_t_subst s acc)
    | Acc a -> Acc (M.a_subst s a)

end

module S1 = Make(Subst.SubstPair)

let acc_t_subst = S1.acc_t_subst

let rec acc_t_to_s (v : Variable.t) : 'a acc_t -> string = function
  | Range (r, acc) ->
      (Serialize.PPrint.r_to_s r) ^ ": " ^ (acc_t_to_s v acc)
  | Cond (b, acc) ->
      "if ( " ^ (Serialize.PPrint.b_to_s b) ^ " ) " ^ (acc_t_to_s v acc)
  | Acc a ->
      Serialize.PPrint.doc_to_string (Serialize.PPrint.acc_expr_to_s (v, a))

let rec get_acc (acc: 'a acc_t) =
  match acc with
  | Range (_, acc)
  | Cond (_, acc) -> get_acc acc
  | Acc a -> a

let proto_to_acc (x:Variable.t) (f: access -> 'a) (p: prog) : 'a acc_t list =
  let rec on_i (i:inst) : 'a acc_t list =
    match i with
    | Acc (y, e) -> if Variable.equal x y then [Acc (f e)] else []
    | Sync -> []
    | Cond (b, is) -> on_p is |> List.map (fun i : 'a acc_t -> (Cond (b, i)))
    | Loop (r, is) -> on_p is |> List.map (fun i : 'a acc_t -> (Range (r, i)))

  and on_p (l:prog) : 'a acc_t list =
    List.concat_map on_i l
  in on_p p


(* ----------------- poly_t type -------------------- *)
module Poly = struct
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
      hashtbl_elements ht
      |> List.map (fun (k, v) -> n_par v ^ " * " ^ x ^ "^" ^ (string_of_int k))
      |> join " + "

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
      hashtbl_elements ht
      |> List.map (fun (i, n) -> (i, n_mult n n1))
      |> hashtbl_from_list
      |> (fun ht -> Many ht)

    | Two {constant=n1; coeficient=n2}, Many ht
    | Many ht, Two {constant=n1; coeficient=n2}
      -> mult (Many (mk_poly_ht n1 n2)) (Many ht)
    | Many ht1, Many ht2 ->
      let ht = Hashtbl.create ((Hashtbl.length ht1) * (Hashtbl.length ht2)) in
      hashtbl_elements ht1
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

    | Many ht -> hashtbl_elements ht
      |> List.map (fun (k, v)-> (k, u_minus v))
      |> fun l -> Many (hashtbl_from_list l)

  let rec from_nexp v (n:nexp) : t =
    match n with
    | Var x -> if x = v then Two {constant=Num 0; coeficient=Num 1} else One n
    | Num _ -> One n
    | Proj _
    | NCall _
    | NIf _ -> One (Num 0)
    | Bin (Plus, e1, e2) -> add (from_nexp v e1) (from_nexp v e2)
    | Bin (Minus, e1, e2) -> add (from_nexp v e1) (uminus (from_nexp v e2))
    | Bin (Mult, e1, e2) -> mult (from_nexp v e1) (from_nexp v e2)
    | Bin _ -> One (Num 0)


end
(*
let proto_to_poly x v p : (Poly.t list) acc_t list =
  proto_to_acc x (fun (a:access) -> List.map (n_to_poly v) (a.access_index)) p
*)
(* ----------------- transaction cost analysis -------------------- *)

(* This function indicates whether our theory CAN analyze the expression, not
   if there are bank-conflicts!  Returns None if we CANNOT analyze. *)
let handle_bank_conflicts (n:nexp) : Poly.t option =
  let handle_coefficient (n:nexp) : bool =
    let fns = Freenames.free_names_nexp n Variable.Set.empty in
    Variable.Set.disjoint (Variable.Set.of_list tid_vars) fns
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
  in List.find_map handle_poly tid_vars

(* p_cost returns bank conflict degree of a poly p *)
let p_cost : Poly.t -> int = function
  (* constant access pattern: this is a broadcast *)
  | One _ -> 1
  (* linear access pattern: maximize degree with Z3 *)
  | Two {constant=_; coeficient=n} ->
    (* we call Z3 from here to calculate the bank conflict degree *)
    let open Z3 in
    let open Z3expr in
    (* print_endline (Freenames.free_names_nexp n); *)
    let ctx = mk_context [] in
    let n_expr = n_to_expr ctx n in
    let k = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "k") in
    let d = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "d") in
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


(* n_cost returns bank conflict degree of a poly n *)
let n_cost (n : nexp) : int =
  let bc_fail (reason : string) : int =
    Printf.eprintf
      "WARNING: %s: %s: assuming worst case bank conflict of %d\n"
      reason (Serialize.PPrint.n_to_s n) num_banks;
    num_banks
  in
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
let access_cost (a : access) : int =
  List.fold_left (+) 0 (List.map n_cost a.access_index)


(* ----------------- kernel cost analysis -------------------- *)


module SymExp = struct
  type t =
    | Const of int
  (*   | Product of Variable.t * nexp * sym_exp *)
    | Sum of Variable.t * nexp * t

  let rec to_string : t -> string =
    function
    | Const x -> string_of_int x
    | Sum (x, n, s) -> "Σ_{" ^ Variable.name x ^ " < " ^ Serialize.PPrint.n_to_s n ^ "} " ^ to_string s

  let rec flatten : t -> nexp =
    function
    | Const k -> Num k
    | Sum (x, ub, s) ->
      (match Poly.from_nexp x (flatten s) with
      | One k -> n_mult ub k
      | Two {constant=c; coeficient=e} ->
        (* S1(e) = (e * (e + 1)) / 2 *)
        n_plus (n_div (n_mult e (n_plus e (Num 1))) (Num 2))
              (n_mult c ub)
      | Many p ->
        failwith ("error: flatten(" ^ Poly.to_string (Variable.name x) (Many p)))
  end


let rec acc_to_sym : access acc_t -> SymExp.t =
  function
  | Acc a -> Const (access_cost a)
  | Cond (_, p) -> acc_to_sym p
  | Range (r, p) ->
    match r with
    | {
        range_var=x;
        range_lower_bound=Num 0;
        range_step = Default (Num 1);
        range_upper_bound=ub;
        _
      } ->
      Sum (x, ub, acc_to_sym p)
    | {range_step = Default (Num 1); _} ->
      (* subst [range_var := range_var + lower_bound]: *)
      let new_range_var = n_plus (Var r.range_var) r.range_lower_bound in
      let p = acc_t_subst (r.range_var, new_range_var) p in
      (* rewrite lower_bound..upper_bound to 0..(upper_bound-lower_bound): *)
      let upper_bound = n_minus r.range_upper_bound r.range_lower_bound in
      Sum (r.range_var, upper_bound, acc_to_sym p)
    | {range_step = Default k; _} ->
      (* x := k (x + lb) *)
      let new_range_var = n_mult (n_plus (Var r.range_var) r.range_lower_bound) k in
      let p = acc_t_subst (r.range_var, new_range_var) p in
      let iters = n_minus r.range_upper_bound r.range_lower_bound in
      (*  (ub-lb)/k + (ub-lb)%k *)
      let upper_bound = n_plus (n_div iters k) (n_mod iters k) in
      Sum (r.range_var, upper_bound, acc_to_sym p)

    | _ -> failwith ("Unsupported range: " ^ Serialize.PPrint.r_to_s r)

(* acc_t_cost returns cost of an acc_t expression *)
let acc_t_cost (acc : access acc_t) : nexp =
  acc
  |> acc_to_sym
  |> (fun x -> print_endline (SymExp.to_string x); x)
  |> SymExp.flatten

(* nexp_sum folds a nexp list into a nexp summation *)
let nexp_sum : nexp list -> nexp =
  List.fold_left n_plus (Num 0)

(* shared_cost returns the cost of all accesses to a shared memory array *)
let shared_cost (k : prog kernel) (v : Variable.t) : nexp =
  let accs : 'a acc_t list = proto_to_acc v Fun.id k.kernel_code in
  nexp_sum (List.map acc_t_cost accs)

(* k_cost returns the cost of a kernel *)
let k_cost (k : prog kernel) : nexp =
  let vs : Variable.t list = Variable.Set.elements (kernel_shared_arrays k) in
  nexp_sum (List.map (shared_cost k) vs)

(* p_k_cost returns the cost of all kernels in the program source *)
let p_k_cost (ks : prog kernel list) : nexp =
  Constfold.n_opt (nexp_sum (List.map k_cost ks))
