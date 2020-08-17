open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil
open Phasealign
(* A global program contains a local program as a base case *)

type 'a phase =
  | Phase of 'a
  | Pre of bexp * 'a phase
  | Global of variable * 'a phase

let rec phase_map (f:'a -> 'b) (p:'a phase) : 'b phase =
  match p with
  | Phase a -> Phase (f a)
  | Pre (b, p) -> Pre (b, phase_map f p)
  | Global (x, p) -> Global (x, phase_map f p)

type p_phase = p_prog phase

type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: p_phase;
}

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let make_global (r:range) (ls:'a phase) : 'a phase =
  Global (r.range_var, Pre (range_to_cond r, ls))

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

(* Implements |> *)
let rec s_prog_to_p_phase: s_prog -> p_phase Stream.t =
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
let prog_to_s_prog (s:Proto.prog) : p_phase Stream.t =
  (* P |> Q1, Q2 *)
  normalize s
  (* phases(P) *)
  |> Streamutil.map n_prog_to_s_prog
  |> Streamutil.map s_prog_to_p_phase
  (* flatten the set *)
  |> Streamutil.concat

(* Inline a set of variables as decls *)
let inline_globals (vars:VarSet.t) (p:'a phase) : 'a phase =
  VarSet.elements vars
  |> List.fold_left (fun p x ->
    Global (x, p)
  ) p

(* Inlines globals and pre-conditon *)
let inline_locals (vars:VarSet.t) (pre:bexp) : p_phase -> p_phase =
  let add_locals (p:'a prog) : 'a prog =
    VarSet.elements vars
    |> List.fold_left (fun p x ->
      [Local (x, p)]
    ) p
  in
  (* Add pre-conditions and global variables *)
  phase_map (fun c -> [Cond (pre, c)] |> add_locals)

let translate (k: Proto.prog kernel) : p_kernel Stream.t =
  prog_to_s_prog k.kernel_code
  |> Streamutil.map (fun p ->
    let p : p_phase = p
      (* Inline pre+locals in each phase *)
      |> inline_locals k.kernel_local_variables k.kernel_pre
      (* Inline globals *)
      |> inline_globals k.kernel_global_variables
    in
    {
      p_kernel_locations = k.kernel_locations;
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

let rec inst_ser (f : 'a -> Sexplib.Sexp.t) : 'a inst -> Sexplib.Sexp.t =
  function
  | Base a -> f a
  | Assert b -> unop "assert" (b_ser b)
  | Cond (b, ls) ->
    prog_ser f ls
    |> s_list
    |> binop "if" (b_ser b)
  | Local (x, ls) ->
    let open Sexplib in
    prog_ser f ls
    |> s_list
    |> binop "local" (Sexp.Atom x.var_name)

and prog_ser (f : 'a -> Sexplib.Sexp.t) (ls: 'a prog) : Sexplib.Sexp.t list =
  List.map (inst_ser f) ls

let rec phase_ser (f: 'a -> Sexplib.Sexp.t) : 'a phase -> Sexplib.Sexp.t list =
  function
  | Phase p -> [f p |> unop "phase"]
  | Pre (b, p) -> unop "pre" (b_ser b) :: (phase_ser f p)
  | Global (x, p) ->
    let open Sexplib in
    unop "global" (Sexp.Atom x.var_name) :: (phase_ser f p)
(*
let p_phase_ser (p: p_phase) : Sexplib.Sexp.t =
  phase_ser (fun x -> prog_ser acc_expr_s x |> s_list) p |> s_list

let kernel_ser (k:p_kernel) : Sexplib.Sexp.t =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    var_set_ser "locations" k.p_kernel_locations;
    unop "code" (p_phase_ser k.p_kernel_code);
  ]
*)
let print_kernels (ks : p_kernel Stream.t) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Stream.iter (fun (k:p_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    PPrint.print_doc (kernel_to_s k)
    (*kernel_ser k |> s_print*)
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