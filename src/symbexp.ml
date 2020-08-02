open Proto
open Subst
open Flatacc

type b_phase = bexp Phasesplit.phase
type b_kernel = b_phase Locsplit.loc_kernel

let proj_accesses (t:task) (h:h_prog) : cond_access list =
  (* Create a hash-table of substitutions make each local variable unique *)
  let make_subst (vars:VarSet.t) : SubstAssoc.t =
    (* Add a suffix to a variable *)
    let var_append (x:variable) (suffix:string) : variable =
      {x with var_name = x.var_name ^ suffix }
    in
    (* Add a suffix to all variables to make them unique. Use $ to ensure
      these variables did not come from C *)
    let suffix = "$" ^ task_to_string t in
    VarSet.elements vars
    |> List.map (fun x -> (x.var_name, Var (var_append x suffix)))
    |> SubstAssoc.make
  in
  (* Find and replace under conditional accesses *)
  let cond_access_subst (s:SubstAssoc.t) ((e,b):cond_access) : cond_access =
    (ReplaceAssoc.a_subst s e, ReplaceAssoc.b_subst s b)
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

let range (i:int) (j:int) : int list =
  let rec iter n acc =
    if n < i then acc else iter (n-1) (n :: acc)
  in
  iter j []

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
    range 0 n
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

let h_kernel_to_b_kernel (k:h_kernel) : bexp =
  Phasesplit.phase_map h_prog_to_bexp k.loc_kernel_code
  |> Phasesplit.normalize_phase ReplacePair.b_subst Proto.VarSet.empty
  |> h_phase_to_bexp

let translate (stream:h_kernel Stream.t) : bexp Stream.t =
  let open Streamutil in
  stream_map h_kernel_to_b_kernel stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : bexp Stream.t) : unit =
  let open Sexplib in
  let open Serialize in
  print_endline "; symbexp";
  let count = ref 0 in
  Stream.iter (fun (b:bexp) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; bool " ^ (string_of_int curr));
    b_ser b |> s_print
  ) ks;
  print_endline "; end of symbexp"
