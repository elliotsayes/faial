open Common
open Exp
open Subst
open Flatacc

type b_phase = bexp Phasesplit.phase

type proof = {
  proof_name: string;
  proof_preds: Predicates.t list;
  proof_decls: string list;
  proof_goal: bexp;
}

let mk_proof (location:variable) (goal:bexp) =
  let open Proto in
  let open Common in
  let decls =
    Freenames.free_names_bexp goal VarSet.empty
    |> VarSet.elements
    |> List.map (fun x -> x.var_name)
  in
  {
    proof_preds = Predicates.get_predicates goal;
    proof_decls = decls;
    proof_goal = goal;
    proof_name = location.var_name;
  }

let proj_accesses (t:task) (h:h_prog) : cond_access list =
  (* Add a suffix to a variable *)
  let var_append (x:variable) (suffix:string) : variable =
    {x with var_name = x.var_name ^ suffix }
  in
  (* Add a suffix to all variables to make them unique. Use $ to ensure
    these variables did not come from C *)
  let task_suffix (t:task) = "$" ^ task_to_string t in
  let proj_var (t:task) (x:variable) : variable =
    var_append x (task_suffix t)
  in
  (* Create a hash-table of substitutions make each local variable unique *)
  let make_subst (vars:VarSet.t) : SubstAssoc.t =
    VarSet.elements vars
    |> List.map (fun x -> (x.var_name, Var (proj_var t x)))
    |> SubstAssoc.make
  in
  let rec inline_proj_n (n: nexp) : nexp =
    match n with
    | Num _
    | Var _ -> n
    | Bin (o, n1, n2) -> Bin (o, inline_proj_n n1, inline_proj_n n2)
    | Proj (t, x) -> Var (proj_var t x)
  in
  let rec inline_proj_b (b: bexp) : bexp =
    match b with
    | Pred _
    | Bool _ -> b
    | BNot b -> BNot (inline_proj_b b)
    | BRel (o, b1, b2) -> BRel (o, inline_proj_b b1, inline_proj_b b2)
    | NRel (o, n1, n2) -> NRel (o, inline_proj_n n1, inline_proj_n n2)
  in
  (* Find and replace under conditional accesses *)
  let cond_access_subst (s:SubstAssoc.t) ((e,b):cond_access) : cond_access =
    (ReplaceAssoc.a_subst s e, inline_proj_b b |> ReplaceAssoc.b_subst s)
  in
  let s = make_subst h.prog_locals in
  List.map (cond_access_subst s) h.prog_accesses

(* The assign_task datatype creates two constructors.
   - assign_mode: given a mode, returns a boolean expression that represents
     assigning the given mode to the current mode-variable.
   - assign_index: given an index and an index value (nexp) returns a boolean
     expression that assigns the expression to the current index.
*)
type assign_task = {
  assign_mode : mode -> bexp;
  assign_index: int -> nexp -> bexp
}

let mode_to_nexp (m:mode) : nexp =
  Num (match m with
  | R -> 0
  | W -> 1)
let mk_var (x:string) = Var (var_make x)
let prefix (t:task) = "$" ^ task_to_string t ^ "$"
let mk_mode (t:task) = mk_var (prefix t ^ "mode")
let mk_idx (t:task) (n:int) = mk_var (prefix t ^ "idx$" ^ string_of_int n)

(* Returns the generators for the given task *)
let mk_task_gen (t:task) =
  let this_mode_v : nexp = mk_mode t in
  let other_mode_v : nexp = mk_mode (other_task t) in
  let idx_v : int -> nexp = mk_idx t in
  let eq_mode (m:mode): bexp =
    let b = n_eq this_mode_v (mode_to_nexp m) in
    if m = R then
      n_eq other_mode_v (mode_to_nexp W)
      |> b_and b
    else b
  in
  {
    assign_mode = eq_mode;
    assign_index = fun idx n -> n_eq (idx_v idx) n;
  }

(* Given a task generator serialize a conditional access *)
let cond_access_to_bexp (t:assign_task) ((e,b):cond_access) :bexp =
  [ b;
    t.assign_mode e.access_mode
  ]
  @
  List.mapi t.assign_index e.access_index
  |> b_and_ex

let cond_acc_list_to_bexp (t:assign_task) (l:cond_access list) : bexp =
  List.map (cond_access_to_bexp t) l
  |> b_or_ex

let h_prog_to_bexp (h:h_prog) : bexp =
  (* Pick one access *)
  let task_to_bexp (t:task) : bexp =
    let gen = mk_task_gen t in
    let accs = proj_accesses t h in
    cond_acc_list_to_bexp gen accs
  in
  (* Make sure all indexeses match *)
  (* $T1$index$0 = $T2$index$0 ... *)
  let gen_eq_index (n:int) : bexp =
    range 0 (n - 1)
    |> List.map (fun i ->
      n_eq (mk_idx Task1 i) (mk_idx Task2 i)
    )
    |> b_and_ex
  in
  (* The dimention is the index count *)
  let rec get_dim (l:cond_access list) : int =
    match l with
    | [] -> failwith "Phase split should not generate empty phases!"
    | (a,_) :: _ -> List.length a.access_index
  in
  b_and_ex [
    task_to_bexp Task1;
    task_to_bexp Task2;
    get_dim h.prog_accesses |> gen_eq_index
  ]

let rec h_phase_to_bexp (h: bexp Phasesplit.phase) : bexp =
  match h with
  | Phase b -> b
  | Pre (b, h) -> h_phase_to_bexp h |> b_and b
  | Global (_, h) -> h_phase_to_bexp h

let h_kernel_to_proof (k:h_kernel) : proof =
  Phasesplit.phase_map h_prog_to_bexp k.loc_kernel_code
  |> Phasesplit.var_uniq_phase ReplacePair.b_subst VarSet.empty
  |> h_phase_to_bexp
  |> Constfold.b_opt (* Optimize the output expression *)
  |> mk_proof k.loc_kernel_location

let translate (stream:h_kernel Stream.t) : proof Stream.t =
  let open Streamutil in
  map h_kernel_to_proof stream

(* ------------------- SERIALIZE ---------------------- *)

let proof_ser (p:proof) : Sexplib.Sexp.t =
  let open Sexplib in
  let open Serialize in
  Sexp.List [
    Sexp.Atom "proof";
    call "decls" (atoms p.proof_decls);
    unop "goal" (b_ser p.proof_goal);
  ]


let print_kernels (ks : proof Stream.t) : unit =
  let open Sexplib in
  let open Serialize in
  print_endline "; symbexp";
  let count = ref 0 in
  Stream.iter (fun (p:proof) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; bool " ^ (string_of_int curr));
    proof_ser p |> s_print
  ) ks;
  print_endline "; end of symbexp"
