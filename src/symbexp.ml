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
    | NIf (b, n1, n2) -> NIf (inline_proj_b b, inline_proj_n n1, inline_proj_n n2)
  and inline_proj_b (b: bexp) : bexp =
    match b with
    | Pred _
    | Bool _ -> b
    | BNot b -> BNot (inline_proj_b b)
    | BRel (o, b1, b2) -> BRel (o, inline_proj_b b1, inline_proj_b b2)
    | NRel (o, n1, n2) -> NRel (o, inline_proj_n n1, inline_proj_n n2)
  in
  (* Find and replace under conditional accesses *)
  let cond_access_subst (s:SubstAssoc.t) (c:cond_access) : cond_access =
    {
      ca_access = ReplaceAssoc.a_subst s c.ca_access;
      ca_cond = inline_proj_b c.ca_cond |> ReplaceAssoc.b_subst s;
      ca_location = c.ca_location;
    }
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
  assign_index: int -> nexp -> bexp;
  assign_loc: Sourceloc.location -> bexp;
}

module LocationHash =
  struct
    type t = Sourceloc.location
    let equal i j : bool = Sourceloc.compare_location i j = 0
    let hash i = Sourceloc.hash_location i
  end

module LocationTbl = Hashtbl.Make(LocationHash)

module LocationCache = struct
  type t = {
    loc_to_int: int LocationTbl.t;
    mutable all_locs: Sourceloc.location list;
  }

  let create (size:int) : t = {
    loc_to_int = LocationTbl.create size;
    all_locs = []
  }

  let get (ht:t) (l:Sourceloc.location) : int =
    match LocationTbl.find_opt ht.loc_to_int l with
    | Some n -> n
    | None ->
      let n = LocationTbl.length ht.loc_to_int in
      LocationTbl.add ht.loc_to_int l n;
      ht.all_locs <- l::ht.all_locs;
      n

  let all (ht:t) : Sourceloc.location list =
    ht.all_locs
end


let mode_to_nexp (m:mode) : nexp =
  Num (match m with
  | R -> 0
  | W -> 1)
let mk_var (x:string) = Var (var_make x)
let prefix (t:task) = "$" ^ task_to_string t ^ "$"
let mk_mode (t:task) = mk_var (prefix t ^ "mode")
let mk_loc (t:task) = mk_var (prefix t ^ "loc")
let mk_idx (t:task) (n:int) = mk_var (prefix t ^ "idx$" ^ string_of_int n)

(* Returns the generators for the given task *)
let mk_task_gen (cache:LocationCache.t) (t:task) : assign_task =
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
  let assign_loc (l:Sourceloc.location) : bexp =
        n_eq (mk_loc (other_task t))
            (Num (LocationCache.get cache l))
  in
  let assign_index (idx:int) (n:nexp) : bexp = n_eq (idx_v idx) n
  in
  {
    assign_mode = eq_mode;
    assign_index = assign_index;
    assign_loc = assign_loc;
  }

(* Given a task generator serialize a conditional access *)
let cond_access_to_bexp (provenance:bool) (t:assign_task) (c:cond_access) :bexp =
    let head = [
        c.ca_cond;
        t.assign_mode c.ca_access.access_mode;
    ] in
    let head = if provenance
        then t.assign_loc c.ca_location :: head
        else head
    in
    head @ List.mapi t.assign_index c.ca_access.access_index
    |> b_and_ex

let cond_acc_list_to_bexp (provenance:bool) (t:assign_task) (l:cond_access list) : bexp =
  List.map (cond_access_to_bexp provenance t) l
  |> b_or_ex

let h_prog_to_bexp (provenance:bool) (cache:LocationCache.t) (h:h_prog) : bexp =
  (* Pick one access *)
  let task_to_bexp (t:task) : bexp =
    let gen = mk_task_gen cache t in
    let accs = proj_accesses t h in
    cond_acc_list_to_bexp provenance gen accs
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
    | c :: _ -> List.length c.ca_access.access_index
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

let f_kernel_to_proof (provenance:bool) (cache:LocationCache.t) (k:f_kernel) : proof =
  { prog_locals = k.f_kernel_local_variables; prog_accesses = k.f_kernel_accesses }
  |> h_prog_to_bexp provenance cache
  |> Constfold.b_opt (* Optimize the output expression *)
  |> mk_proof k.f_kernel_location

let translate2 (provenance:bool) (stream:f_kernel Streamutil.stream) : (LocationCache.t * proof Streamutil.stream) =
  let open Streamutil in
  let c = LocationCache.create 100 in
  c, map (f_kernel_to_proof provenance c) stream

let h_kernel_to_proof (provenance:bool) (cache:LocationCache.t) (k:h_kernel) : proof =
  Phasesplit.phase_map (h_prog_to_bexp provenance cache) k.loc_kernel_code
  |> Phasesplit.var_uniq_phase ReplacePair.b_subst VarSet.empty
  |> h_phase_to_bexp
  |> Constfold.b_opt (* Optimize the output expression *)
  |> mk_proof k.loc_kernel_location

let translate (provenance:bool) (stream:h_kernel Streamutil.stream) : (LocationCache.t * proof Streamutil.stream) =
  let open Streamutil in
  let c = LocationCache.create 100 in
  c, map (h_kernel_to_proof provenance c) stream

(* ------------------- SERIALIZE ---------------------- *)

let proof_ser (p:proof) : Smtlib.sexp =
  let open Smtlib in
  let open Serialize in
  List [
    symbol "proof";
    call "decls" (atoms p.proof_decls);
    unop "goal" (b_ser p.proof_goal);
  ]

let proof_to_s (p:proof) : Serialize.PPrint.t list =
  let open Common in
  let open Serialize in
  let open PPrint in
  let preds =
    let open Predicates in
    List.map (fun x -> x.pred_name) p.proof_preds
    |> join ", "
  in
  [
      Line ("array: " ^ p.proof_name);
      Line ("predicates: " ^ preds ^ ";");
      Line ("decls: " ^ (p.proof_decls |> join ", ") ^ ";");
      Line ("goal: " ^ b_to_s p.proof_goal ^ ";");
  ]



let print_kernels ((cache, ks) : LocationCache.t * proof Streamutil.stream) : unit =
  let open Serialize in
  print_endline "; symbexp";
  let count = ref 0 in
  Streamutil.iter (fun (p:proof) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; bool " ^ (string_of_int curr));
    proof_to_s p |> Serialize.PPrint.print_doc
  ) ks;
  print_endline "; end of symbexp"
