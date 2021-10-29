open Exp
open Proto
open Common


(* ----------------- constants -------------------- *)

let tid_vars : variable list =
  List.map var_make
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

let rec acc_t_to_s (v : variable) : 'a acc_t -> string = function
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

let proto_to_acc (x:variable) (f: access -> 'a) (p: prog) : 'a acc_t list = 
  let rec on_i (i:inst) : 'a acc_t list =
    match i with
    | Acc (y, e) -> if var_equal x y then [Acc (f e)] else []
    | Sync -> []
    | Cond (b, is) -> on_p is |> List.map (fun i -> (Cond (b, i)))
    | Loop (r, is) -> on_p is |> List.map (fun i -> (Range (r, i)))

  and on_p (l:prog) : 'a acc_t list =
    List.concat_map on_i l
  in on_p p


(* ----------------- poly_t type -------------------- *)

type poly_ht = (int, nexp) Hashtbl.t

type poly_t =
  | One of nexp
  | Two of nexp * nexp
  | Many of poly_ht

let poly_to_string x (p:poly_t) =
  let open Serialize in
  let open PPrint in
  match p with
  | One n -> n_to_s n
  | Two (n1, n2) -> n_par n1 ^ " + " ^ n_par n2 ^ " * " ^ x
  | Many ht ->
    hashtbl_elements ht
    |> List.map (fun (k, v) -> n_par v ^ " * " ^ x ^ "^" ^ (string_of_int k))
    |> join " + "

let make_poly e n =
  if n = 0 then
    One e
  else if n = 1 then
    Two (Num 0, e)
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

let poly_add e1 e2 =
  match e1, e2 with
  | One n1, One n2 -> One (n_plus n1 n2)
  | One n1, Two (n2, n3)
  | Two (n2, n3), One n1 ->
    Two (n_plus n2 n1, n3)
  | Two (n1, n2), Two (n3, n4) -> Two (n_plus n1 n3, n_plus n2 n4)
  | One n1, Many ht
  | Many ht, One n1 ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (n_plus n1);
    Many ht
  | Two (n1, n2), Many ht
  | Many ht, Two (n1, n2) ->
    let ht = Hashtbl.copy ht in
    poly_update_ht ht 0 (n_plus n1);
    poly_update_ht ht 1 (n_plus n2);
    Many ht
  | Many ht1, Many ht2 ->
    let ht2 = Hashtbl.copy ht2 in
    poly_add_ht ht1 ht2;
    Many ht2

let rec poly_mult e1 e2 =
  let poly_mult_ht (src:poly_ht) ((i1,n1):int*nexp) : poly_ht =
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
  | One n1, One n2 -> One (n_mult n1 n2)
  | One n1, Two (n2, n3)
  | Two (n2, n3), One n1
    -> Two (n_mult n1 n2, n_mult n1 n3)
  | Two (n1, n2), Two (n3, n4) ->
      let ht' = poly_mult_ht (mk_poly_ht n3 n4) (1, n2) in
      poly_add (poly_mult (One n1) e2) (Many ht')
  | One n1, Many ht
  | Many ht, One n1 ->
    hashtbl_elements ht
    |> List.map (fun (i, n) -> (i, n_mult n n1))
    |> hashtbl_from_list
    |> (fun ht -> Many ht)
  | Two (n1, n2), Many ht
  | Many ht, Two (n1, n2)
    -> poly_mult (Many (mk_poly_ht n1 n2)) (Many ht)
  | Many ht1, Many ht2 ->
    let ht = Hashtbl.create ((Hashtbl.length ht1) * (Hashtbl.length ht2)) in
    hashtbl_elements ht1
    |> List.map (poly_mult_ht ht2)
    |> List.iter (fun src ->
      poly_add_ht src ht
    );
    Many ht

let poly_uminus (p:poly_t) : poly_t =
  let u_minus n = n_mult (Num (-1)) n in
  match p with
  | One n -> One (u_minus n)
  | Two (n1, n2) -> Two (u_minus n1, u_minus n2)
  | Many ht -> hashtbl_elements ht
    |> List.map (fun (k, v)-> (k, u_minus v))
    |> fun l -> Many (hashtbl_from_list l)

let rec n_to_poly v (n:nexp) : poly_t =
  match n with
  | Var x -> if x = v then Two (Num 0, Num 1) else One n
  | Num _ -> One n
  | Proj _
  | NCall _
  | NIf _ -> One (Num 0)
  | Bin (Plus, e1, e2) -> poly_add (n_to_poly v e1) (n_to_poly v e2)
  | Bin (Minus, e1, e2) -> poly_add (n_to_poly v e1) (poly_uminus (n_to_poly v e2))
  | Bin (Mult, e1, e2) -> poly_mult (n_to_poly v e1) (n_to_poly v e2)
  | Bin _ -> One (Num 0)


let proto_to_poly x v p : (poly_t list) acc_t list =
  proto_to_acc x (fun (a:access) -> List.map (n_to_poly v) (a.access_index)) p


(* ----------------- kernel functions -------------------- *)

let open_ic_with (fname:string option) (f : in_channel -> unit) : unit =
    let ic, (closer: in_channel -> unit) = match fname with
    | Some fname -> (open_in fname, close_in_noerr)
    | None -> (stdin, fun x -> ())
    in
    try (f ic; closer ic) with
    | e -> closer ic;
      raise e


let p_kernel_parser fname input : prog kernel =
  let fname = match fname with
  | Some x -> x
  | None -> "<STDIN>"
  in
  let filebuf = Lexing.from_channel input in
  Scan.set_filename filebuf fname;
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    let b = Buffer.create 1024 in
    let sloc = Sourceloc.of_lexbuf filebuf in
    Printf.bprintf b "%a: syntax error" Sourceloc.location_bprint_start sloc;
    (try
        Printf.bprintf b "%a" Sourceloc.location_bprint_title sloc
    with
        Sys_error _ -> ()
    );
    raise (Common.ParseError b)

let j_kernel_parser (_:string option) ic =
  try Yojson.Basic.from_channel ic |> Parsejs.parse_kernels.run with
  | Yojson.Json_error("Blank input data") ->
    (* If the input is blank, just ignore the input and err with -1 *)
    raise (Common.mk_parse_error_s "Empty input data. Blank file?\n")
  | Yojson.Json_error(e) ->
    raise (Common.mk_parse_error_s (Printf.sprintf "Error parsing JSON: %s\n" e))

type i_kernel =
  | JKernel of Imp.p_kernel list
  | PKernel of prog kernel

let parse_i_kernel (use_json:bool) (fname:string option) (ic:in_channel) : i_kernel =
  if use_json then
    JKernel (j_kernel_parser fname ic)
  else
    PKernel (p_kernel_parser fname ic)

let open_i_kernel_with (use_json:bool) (fname:string option) (f:i_kernel -> unit) : unit =
  open_ic_with fname (fun ic ->
    f (parse_i_kernel use_json fname ic)
  )

let i_kernel_to_p_kernel (k:i_kernel) : prog kernel list =
  match k with
  | JKernel ks -> List.map Imp.compile ks
  | PKernel p -> [p]


(* ----------------- transaction cost analysis -------------------- *)

(* This function indicates whether our theory CAN analyze the expression, not
   if there are bank-conflicts!  Returns None if we CANNOT analyze. *)
let handle_bank_conflicts (n:nexp) : poly_t option =
  let handle_coefficient (n:nexp) : bool =
    let fns = Freenames.free_names_nexp n VarSet.empty in
    VarSet.disjoint (VarSet.of_list tid_vars) fns
  in
  let handle_poly (x: variable) : poly_t option =
    let p = n_to_poly x n in
    match p with
    | One n ->
      (* var x (e.g., threadIdx.x) is not in the expression *)
      if handle_coefficient n then Some p else None
    | Two (c, k) ->
      (* The expression is of form: (k * x + c) *)
      if handle_coefficient c && handle_coefficient k
      then Some p else None
    | Many _ -> None
  in List.find_map handle_poly tid_vars

(* p_cost returns bank conflict degree of a poly p *)
let p_cost : poly_t -> int = function
  (* constant access pattern: this is a broadcast *)
  | One _ -> 1
  (* linear access pattern: maximize degree with Z3 *)
  | Two (_, n) ->
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
  match handle_bank_conflicts n with
  | Some p ->
    begin try p_cost p with
    | Z3expr.Not_implemented e -> bc_fail e (* Z3expr TODO *)
    end
  | None -> bc_fail "pattern not linear"    (* theory TODO *)

(* access_cost returns bank conflict cost of an access *)
let access_cost (a : access) : int =
  List.fold_left (+) 0 (List.map n_cost a.access_index)


(* ----------------- kernel cost analysis -------------------- *)

(* normalize ranges in acc to start with zero *)
let rec normalize_ranges (acc: 'a acc_t) : 'a acc_t =
  match acc with
  (* TODO: check range for tid in step before normalizing *)
  | Range (r, acc) ->
      (* subst [range_var := range_var + lower_bound]: *)
      let new_range_var = Bin (Plus, Var r.range_var, r.range_lower_bound) in
      let acc = acc_t_subst (r.range_var, new_range_var) acc in
      (* rewrite lower_bound..upper_bound to 0..(upper_bound-lower_bound): *)
      let r : range = {
        range_var = r.range_var;
        range_lower_bound = Num 0;
        range_upper_bound = Bin (Minus, r.range_upper_bound, r.range_lower_bound);
        range_step = r.range_step } in
      (* the resulting normalized range: *)
      Range (r, normalize_ranges acc)
  | Cond (b, acc) -> Cond (b, normalize_ranges acc)
  | Acc _ -> acc

(* acc_t_cost returns cost of an acc_t expression *)
let rec acc_t_cost (acc : 'a acc_t) : nexp =
  let acc = normalize_ranges acc in
  match acc with
  | Range (r, acc) ->
    begin match r.range_step with
    | Default step ->
        let lb = r.range_lower_bound in
        let ub = r.range_upper_bound in
          (*  ((ub - lb) / step) * (cost of acc):  *)
          Bin (Mult, Bin (Div, Bin (Minus, ub, lb), step), acc_t_cost acc)
    | StepName pred_name ->
        Printf.eprintf "WARNING: range step %s unsupported in range %s\n"
          pred_name (Serialize.PPrint.r_to_s r);
        acc_t_cost acc
        (* TODO: support more pred_name steps *)
    end
  | Cond (b, acc) -> acc_t_cost acc
  | Acc a -> Num (access_cost a)

(* nexp_sum folds a nexp list into a nexp summation *)
let nexp_sum : nexp list -> nexp =
  List.fold_left (fun n1 n2 -> Bin (Plus, n1, n2)) (Num 0)

(* shared_cost returns the cost of all accesses to a shared memory array *)
let shared_cost (k : prog kernel) (v : variable) : nexp =
  let accs : 'a acc_t list = proto_to_acc v Fun.id k.kernel_code in
  nexp_sum (List.map acc_t_cost accs)

(* k_cost returns the cost of a kernel *)
let k_cost (k : prog kernel) : nexp =
  let vs : variable list = VarSet.elements (kernel_shared_arrays k) in
  nexp_sum (List.map (shared_cost k) vs)

(* i_k_cost returns the cost of all kernels in the program source *)
let i_k_cost (k : i_kernel) : nexp =
  let ks : prog kernel list = i_kernel_to_p_kernel k in
  Constfold.n_opt (nexp_sum (List.map k_cost ks))


(* ----------------- execution entry point -------------------- *)

let () =
  let print_cost (k : i_kernel) : unit =
    print_endline (Serialize.PPrint.n_to_s (i_k_cost k))
  in
  try open_i_kernel_with true None print_cost with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)
