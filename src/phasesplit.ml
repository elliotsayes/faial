open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil
open Phasealign
open Hash_rt
open Ppx_compare_lib.Builtin (* compare_list *)
(* A global program contains a local program as a base case *)

type 'a phase =
  | Phase of 'a
  | Pre of bexp * 'a phase
  | Global of variable * 'a phase
  [@@deriving hash, compare]

let rec get_phase (ph:'a phase) : 'a =
  match ph with
  | Phase a -> a
  | Pre (_, ph) -> get_phase ph
  | Global (_, ph) -> get_phase ph

let rec phase_map (f:'a -> 'b) (p:'a phase) : 'b phase =
  match p with
  | Phase a -> Phase (f a)
  | Pre (b, p) -> Pre (b, phase_map f p)
  | Global (x, p) -> Global (x, phase_map f p)

let rec free_names_phase (p:'a phase) (fns:VarSet.t) : VarSet.t =
  match p with
  | Phase a -> fns
  | Pre (b, p) -> Freenames.free_names_bexp b fns |> free_names_phase p
  | Global (x, p) -> free_names_phase p fns |> VarSet.remove x
(*
type l_inst =
  | LAcc of acc_expr
  | LCond of bexp * l_inst list
   [@@deriving hash, compare]

type l_prog = {
  l_locals: variable list;
  l_code: l_inst list;
 } [@@deriving hash, compare]
*)
type u_phase = u_prog phase [@@deriving hash, compare]

type p_phase = p_prog phase [@@deriving hash, compare]

let rec get_locs (p:p_phase) =
  match p with
  | Phase p -> Phasealign.get_locs VarSet.empty p
  | Pre (_, p) | Global (_, p) -> get_locs p

type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: p_phase;
}

type u_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  u_kernel_locations: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  u_kernel_global_variables: VarSet.t;
  (* The internal variables are used in the code of the kernel.  *)
  u_kernel_local_variables: VarSet.t;
  (* Global ranges *)
  u_kernel_ranges: range list;
  (* The code of a kernel performs the actual memory accesses. *)
  u_kernel_code: u_inst;
  (* A thread-local pre-condition that is true on all phases. *)
}

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let make_global (r:range) (ls:'a phase) : 'a phase =
  Global (r.range_var, Pre (range_to_cond r, ls))

(* -------------------- HASHING ------------------------------------ *)

module PPhaseHash =
  struct
    type t = p_phase
    let equal i j : bool = compare_p_phase i j = 0
    let hash i = hash_p_phase i
  end

module PPhaseHashtbl = Hashtbl.Make(PPhaseHash)

(* ---------------- SUBSTITUTION ----------------------------------- *)

let phase_subst (f:SubstPair.t -> 'a -> 'a) (s:SubstPair.t) (p:'a phase) : 'a phase =
  let rec subst (s:SubstPair.t) (i:'a phase) : 'a phase =
    match i with
    | Phase b -> Phase (f s b)
    | Pre (b, p1) -> Pre (
        ReplacePair.b_subst s b,
        subst s p1
      )
    | Global (x, p) ->
      ReplacePair.add s x (function
        | Some s -> Global (x, subst s p)
        | None -> i
      )
  in
  subst s p

(* ---------------- MAKE VARIABLES DISTINCT -------------------------------- *)


(* Make variables distinct. *)

let var_uniq_phase (f:SubstPair.t -> 'a -> 'a) (known:VarSet.t) (p:'a phase) : 'a phase =
  let open Bindings in
  let rec norm (i:'a phase) (xs:VarSet.t) : 'a phase * VarSet.t =
    match i with
    | Global (x, p) ->
      if VarSet.mem x xs then (
        let new_x : variable = generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let s = Subst.SubstPair.make (x, Var new_x) in
        let new_p = phase_subst f s p in
        let (p, new_xs) = norm new_p new_xs in
        Global (new_x, p), new_xs
      ) else (
        let (p, new_xs) = norm p (VarSet.add x xs) in
        Global (x, p), new_xs
      )
    | Pre (b, p) ->
      let (p, xs) = norm p xs in
      (Pre (b, p), xs)
    | Phase _ -> i, xs
  in
  norm p known |> fst

(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

type barrier_interval = {
  bi_code: u_inst;
  bi_ranges: range list;
}

let bi_add (bi:barrier_interval) (r:range) : barrier_interval =
  { bi with bi_ranges = r :: bi.bi_ranges }

(* Implements |> *)
let a_prog_to_bi (pre:bexp) : a_prog -> barrier_interval stream =
  let rec i_phase: a_inst -> barrier_interval stream =
    function
      (* ^P; sync |> { P } *)
    | ASync u ->
      {
        bi_code = UCond (pre, u);
        bi_ranges = []
      }
      |> Streamutil.one
    | ALoop (p, r, q) ->
      (* Rule:
        P |> p    q = { for x in [n,m) Q | Q \in p }
        ------------------------------
        for x in [n,m) {Q} |> q
      *)
      (* Break down the body into phases, and prefix each phase with the
          binding *)
      p_phase p
      |> Streamutil.sequence (
        p_phase q
        |> Streamutil.map (fun bi ->
          (* For every phase in q, prefix it with variable in r *)
          bi_add bi r
        )
      )
  and p_phase : a_prog -> barrier_interval stream =
    function
    | [] -> Streamutil.empty
    | i :: p ->
      (* Rule:
        P |> p      Q |> q
        ------------------
        P;Q |> p U q
        *)
      i_phase i
      |> Streamutil.sequence (p_phase p)
  in
  p_phase

let u_free_names (p:u_prog) : VarSet.t -> VarSet.t =
  let rec fn_i (i:u_inst) (fns:VarSet.t) : VarSet.t =
    match i with
    | UAcc (_,e) ->
      Freenames.free_names_access e fns
    | ULoop (r, l) ->
      Freenames.free_names_range r fns |> fn_p l
    | UCond (b, l) ->
      Freenames.free_names_bexp b fns |> fn_p l
  and fn_p (p:u_prog) (fns:VarSet.t) : VarSet.t =
    List.fold_right fn_i p fns
  in
  fn_p p
(*
let u_prog_to_u_prog (pre:bexp) (locals:VarSet.t) (p:u_prog) : u_prog =
  (* We want to make each phase self-contained. This means that we need to
     somehow declare in the code the locals that are defiend at the parameter
     level.

     The problem is that u_prog only allows declaring variables with a loop.
     Thus, we create a new language, called l_prog, that defines all locals
     in a list, and holds the code after that.
 *)

  let rec i_to_l : u_inst -> l_inst = function
    | UAcc e -> LAcc e
    | UCond (b, l) -> LCond (b, p_to_l l)
    | ULoop (r, l) -> LCond (range_to_cond r, p_to_l l)
  and p_to_l : u_prog -> l_inst list = function
    | i :: l -> i_to_l i :: p_to_l l
    | [] -> []
  in
  let rec fn_i (i:l_inst) (n:VarSet.t) : VarSet.t =
    match i with
    | LAcc (_,e) -> Freenames.free_names_access e n
    | LCond (b, l) -> Freenames.free_names_bexp b n |> fn_p l
  and fn_p (l: l_inst list) (n: VarSet.t) : VarSet.t =
    List.fold_right fn_i l n
  in
  let p = [LCond(pre, p_to_l p)] in
  let locals = VarSet.inter (fn_p p VarSet.empty) locals in
  {
    (* We sort the list of locals so that we can hash regardless of the order *)
    l_locals = VarSet.elements locals |> List.sort Exp.compare_variable;
    l_code = p;
  }
*)

(* Inline a set of variables as decls *)
let inline_globals (vars:VarSet.t) (p:'a phase) : 'a phase =
  VarSet.elements vars
  |> List.fold_left (fun p x ->
    Global (x, p)
  ) p

exception PhasesplitException of (string * Sourceloc.location) list

let translate2 (k: a_prog kernel) (expect_typing_fail:bool) : u_kernel stream =
  let p_to_k ((bi,locations):(barrier_interval * VarSet.t)) : u_kernel =
    (* Check for undefs *)
    (* 1. compute all globals *)
    let globals =
      List.map (fun r -> r.range_var) bi.bi_ranges
      |> VarSet.of_list
      |> VarSet.union k.kernel_global_variables
    in
    (* 2. compute all free names in the ranges *)
    let fns = List.fold_right Freenames.free_names_range bi.bi_ranges VarSet.empty in
    (* 3. check if there are any locals *)
    let errs = VarSet.diff fns globals
      |> VarSet.elements
      |> List.map (fun (x:variable) ->
        "Barrier divergence error: cannot use thread-local variable '" ^
        x.var_name ^ "' in synchronized control flow",
        x.var_loc
        )
    in
    if List.length errs > 0 then
      raise (PhasesplitException errs)
    else
      {
        u_kernel_local_variables = k.kernel_local_variables;
        u_kernel_global_variables = k.kernel_global_variables;
        u_kernel_locations = locations;
        u_kernel_ranges = bi.bi_ranges;
        u_kernel_code = bi.bi_code;
      }
  in
  a_prog_to_bi k.kernel_pre k.kernel_code
  |> map_opt (fun b ->
    (* Get locations of u_prog *)
    let locations:VarSet.t = Phasealign.get_locs2 [b.bi_code] VarSet.empty in
    if VarSet.is_empty locations then None
    else Some (b, locations)
  )
  |> Streamutil.map p_to_k


(* Implements |> *)
let rec s_prog_to_p_phase: s_prog -> p_phase stream =
  function
    (* ^P; sync |> { P } *)
  | NPhase u -> Phase u |> Streamutil.one
(*
  | NCond (b, p) ->
    (* Rule:
      P |> p
      ------------------------------
      if (b) P |> { if (b) Q | Q \in p }
     *)
    s_prog_to_p_phase p
    |> Streamutil.map (fun p' -> Pre (b, p'))
    *)
  | NSeq (p, q) ->
    (* Rule:
       P |> p      Q |> q
       ------------------
       P;Q |> p U q
       *)
    s_prog_to_p_phase p
    |> Streamutil.sequence (s_prog_to_p_phase q)
  | NFor (r, p) ->
    (* Rule:
      P |> p    q = { for x in [n,m) Q | Q \in p }
      ------------------------------
      for x in [n,m) {Q} |> q
     *)
    (* Break down the body into phases, and prefix each phase with the
        binding *)
    s_prog_to_p_phase p
    |> Streamutil.map (fun q ->
      (* For every phase in q, prefix it with variable in r *)
      make_global r q
    )

let n_prog_to_s_prog : n_prog -> s_prog =
  function
  | Open u -> NPhase u
  | Unaligned (p, q) -> NSeq (p, NPhase q)

(* Takes a program with Syncs and generates a program with phased blocks *)
let prog_to_s_prog (s:Proto.prog) : p_phase stream =
  let known = PPhaseHashtbl.create 100 in
  let keep_phase (p:p_phase) : bool =
    if PPhaseHashtbl.mem known p then
      false
    else begin
      (* Cache element *)
      PPhaseHashtbl.add known p ();
      true
    end
  in
  (* P |> Q1, Q2 *)
  normalize s
  (* phases(P) *)
  |> Streamutil.map n_prog_to_s_prog
  |> Streamutil.map s_prog_to_p_phase
  (* flatten the set *)
  |> Streamutil.concat
  |> Streamutil.filter keep_phase

(* Inlines locals and pre-conditon *)
let inline_locals (vars:VarSet.t) (pre:bexp) : p_phase -> p_phase =
  let add_locals (p:'a prog) : 'a prog =
    VarSet.elements vars
    |> List.fold_left (fun p x ->
      [Local (x, p)]
    ) p
  in
  (* Add pre-conditions and global variables *)
  phase_map (fun c -> [Cond (pre, c)] |> add_locals)

let translate (k: Proto.prog kernel) (expect_typing_fail:bool) : p_kernel stream =
  prog_to_s_prog k.kernel_code
  |> Streamutil.map (fun p ->
    let p : p_phase = p
      (* Inline pre+locals in each phase *)
      |> inline_locals k.kernel_local_variables k.kernel_pre
      (* Inline globals *)
      |> inline_globals k.kernel_global_variables
    in
    let errs = free_names_phase p VarSet.empty
      |> VarSet.elements
      |> List.map (fun (x:variable) ->
        "Barrier divergence error: cannot use thread-local variable '" ^ x.var_name ^ "' in synchronized control flow",
          x.
        var_loc
      )
    in
    if List.length errs > 0 then
      raise (PhasesplitException errs)
    else
      {
        p_kernel_locations = get_locs p;
        p_kernel_code = p
      }
  )

(* --------------------------- PRETTY PRINT ------------------- *)

let prog_to_s (f: 'a -> PPrint.t list) : 'a prog -> PPrint.t list =
  let open PPrint in
  let rec inst_s : 'a inst -> t list =
    function
    | Base a -> f a
    | Assert b -> [Line ("assert " ^ b_to_s b)]
    | Cond (b, p) ->
      [
        Line ("if (" ^ b_to_s b ^ ") {");
        Block (prog_s p);
        Line ("}");
      ]
    | Local (x, p) ->
      Line ("local " ^ ident x ^ ";")
      :: prog_s p

  and prog_s (p: 'a prog) : t list =
    List.map inst_s p |> List.flatten
  in
  prog_s

let rec phase_to_s (f: 'a -> PPrint.t list) : 'a phase -> PPrint.t list =
  let open PPrint in
  function
  | Phase p -> f p
  | Pre (b, p) ->
      Line ("assert* (" ^ b_to_s b ^ ");")
      ::
      phase_to_s f p
  | Global (x, p) ->
      Line ("global " ^ ident x ^ ";") ::
      phase_to_s f p

let p_phase_to_s: p_phase -> PPrint.t list =
  phase_to_s (prog_to_s PPrint.acc_expr_to_s)

let kernel_to_s (k:p_kernel) : PPrint.t list =
  let open PPrint in
  [
      Line ("locations: " ^ var_set_to_s k.p_kernel_locations ^ ";");
      Line "code {";
      Block (p_phase_to_s k.p_kernel_code);
      Line "}"
  ]

(* ---------------------- SERIALIZATION ------------------------ *)

let rec inst_ser (f : 'a -> Smtlib.sexp) : 'a inst -> Smtlib.sexp =
  function
  | Base a -> f a
  | Assert b -> unop "assert" (b_ser b)
  | Cond (b, ls) ->
    prog_ser f ls
    |> s_list
    |> binop "if" (b_ser b)
  | Local (x, ls) ->
    prog_ser f ls
    |> s_list
    |> binop "local" (symbol x.var_name)

and prog_ser (f : 'a -> Smtlib.sexp) (ls: 'a prog) : Smtlib.sexp list =
  List.map (inst_ser f) ls

let rec phase_ser (f: 'a -> Smtlib.sexp) : 'a phase -> Smtlib.sexp list =
  function
  | Phase p -> [f p |> unop "phase"]
  | Pre (b, p) -> unop "pre" (b_ser b) :: (phase_ser f p)
  | Global (x, p) ->
    unop "global" (symbol x.var_name) :: (phase_ser f p)

let print_kernels (ks : p_kernel Streamutil.stream) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Streamutil.iter (fun (k:p_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    PPrint.print_doc (kernel_to_s k)
  ) ks;
  print_endline "; end of conc"

let u_kernel_to_s (k:u_kernel) : PPrint.t list =
  let open PPrint in
  let ranges =
    List.map r_to_s k.u_kernel_ranges
    |> join "; "
  in
  [
      Line ("locations: " ^ var_set_to_s k.u_kernel_locations ^ ";");
      Line ("globals: " ^ var_set_to_s k.u_kernel_global_variables ^ ";");
      Line ("locals: " ^ var_set_to_s k.u_kernel_local_variables ^ ";");
      Line ("ranges: " ^ ranges ^ ";");
      Line "{";
      Block (u_inst_to_s k.u_kernel_code);
      Line "}"
  ]


let print_kernels2 (ks : u_kernel Streamutil.stream) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Streamutil.iter (fun (k:u_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    PPrint.print_doc (u_kernel_to_s k)
  ) ks;
  print_endline "; end of conc"

(*
  let rec run (s:t) =
    let rec run_aux (s:t) (accum,phase) =
      match s with
      | (Loop (var,r,t1))::l ->
          let i = eval_expr E.to_int r.r_lowerbound in
          let j = eval_expr E.to_int r.r_upperbound in
          let subbed_t1 = a_subst var (E.to_expr i) t1 in
          let t1' = a_subst var (Incr (E.to_expr i)) t1 in
          if i < j then
            (
              let r' = createRange (Incr (E.to_expr i)) r.r_upperbound in
              run_aux (subbed_t1@(Loop (var,r',t1'))::l) (accum,phase)
            )
          else run_aux l (accum,phase)
      | (Codeline (n,c))::l ->
          let n' = eval_expr E.to_int n in
          run_aux l (accum,(E.to_expr n')::phase)
      | Sync::l -> run_aux l (phase::accum,[])
      | [] -> (accum,phase)
    in
    let (accum,phase) = run_aux s ([],[]) in
    let ret = phase::accum in
    let ret = List.rev ret in
    List.map List.rev ret
*)