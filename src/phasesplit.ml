open Exp
open Proto
open Common
open Serialize
open Subst
open Streamutil

type 'a inst =
  | Base of 'a
  | Assert of bexp
  | Cond of bexp * 'a inst list
  | Local of variable * 'a inst list

type 'a prog = 'a inst list

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

type p_inst = Proto.acc_inst inst
type p_prog = Proto.acc_inst prog
type p_phase = p_prog phase
type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: p_phase;
}

(* -------------------- UTILITY CONSTRUCTORS ---------------------- *)

let range_to_cond (r:range) =
  b_and
    (n_le r.range_lower_bound (Var r.range_var))
    (n_lt (Var r.range_var) r.range_upper_bound)

let make_local (r:range) (ls:'a prog) : 'a inst =
  Local (r.range_var, [Cond (range_to_cond r, ls)])

let make_global (r:range) (ls:'a phase) : 'a phase =
  Global (r.range_var, Pre (range_to_cond r, ls))


(* ---------------- SUBSTITUTION ----------------------------------- *)


let prog_subst (f:SubstPair.t -> 'a -> 'a) (s:SubstPair.t) (p:'a prog) : 'a prog =
  let rec i_subst (s:SubstPair.t) (i:'a inst) : 'a inst =
    match i with
    | Base b -> Base (f s b)
    | Assert b -> Assert (ReplacePair.b_subst s b)
    | Cond (b, p1) -> Cond (
        ReplacePair.b_subst s b,
        p_subst s p1
      )
    | Local (x, p) ->
      ReplacePair.add s x (function
        | Some s -> Local (x, p_subst s p)
        | None -> i
      )
  and p_subst (s:SubstPair.t) : 'a prog  -> 'a_prog =
    List.map (i_subst s)
  in
  p_subst s p

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

(* ---------------- MAKE UNIQUE ----------------------------------- *)


(* Make variables distinct. *)

let var_uniq_prog (f:SubstPair.t -> 'a -> 'a) (known:VarSet.t) (p:'a prog) : 'a prog =
  let open Bindings in
  let rec norm_inst (i:'a inst) (xs:VarSet.t) : 'a inst * VarSet.t =
    match i with
    | Local (x, p) ->
      if VarSet.mem x xs then (
        let new_x : variable = generate_fresh_name x xs in
        let new_xs = VarSet.add new_x xs in
        let s = Subst.SubstPair.make (x, Var new_x) in
        let new_p = prog_subst f s p in
        let (p, new_xs) = norm_prog new_p new_xs in
        Local (new_x, p), new_xs
      ) else (
        let (p, new_xs) = norm_prog p (VarSet.add x xs) in
        Local (x, p), new_xs
      )
    | Cond (b, p) ->
      let (p, xs) = norm_prog p xs in
      (Cond (b, p), xs)
    | Assert _
    | Base _ -> i, xs
  and norm_prog (p:'a prog) (xs:VarSet.t) : 'a prog * VarSet.t =
    match p with
    | [] -> ([], xs)
    | i::p ->
      let (i, xs) = norm_inst i xs in
      let (p, xs) = norm_prog p xs in
      (i::p, xs)
  in
  norm_prog p known |> fst

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

(* ---------------- FIST STAGE OF TRANSLATION ---------------------- *)

let p_subst : SubstPair.t -> p_prog -> p_prog = prog_subst ReplacePair.acc_inst_subst

let ph_subst : SubstPair.t -> p_phase -> p_phase = phase_subst p_subst

(* Prepend an instruction to the closest phase *)

let rec prepend (u:p_prog) : p_phase -> p_phase =
  function
    | Phase u' -> Phase (u @ u')
    | Pre (b, p) ->
      Pre (b, prepend u p)
    | Global (r, p) -> Global (r,prepend u p)

(* Represents |> *)

(* Represents the 4 |> rules for sequence*)

let seq (p:p_phase option * p_prog) (q:p_phase option * p_prog) : (p_phase option * p_prog) Stream.t  =
  match p, q with
  (* Rule: ^P |> _|_, P *)
  | (None,p2),(None,q2) -> (None, (p2@q2)) |> Streamutil.one
  (* Rule:
   ^P    Q |> Q1, Q2
   -----------------
   P;Q |> (P;Q1), Q2
   *)
  | (None,p2),(Some q1, q2) -> (Some (prepend p2 q1),q2) |> Streamutil.one
  (* Rule:
    P |> P1, P2      ^Q
    -------------------
    P;Q |> P1, (P2,Q)
    *)
  | (Some p1,p2),(None, q2) -> (Some p1,(p2@q2)) |> Streamutil.one
  | (Some p1,p2),(Some q1, q2) ->
    [
      (* Rule:
       P |> P1, P2     Q |> Q1, Q2
       ---------------------------
       P;Q |> P1, skip
       *)
      Some p1, [];
      (* Rule:
       P |> P1, P2     Q |> Q1, Q2
       ---------------------------
       P;Q |> (P2;Q), Q2
       *)
      Some (prepend p2 q1), q2;
    ] |> Stream.of_list

(* Typesafe normalization *)
let rec normalize1 : Proto.inst -> (p_phase option * p_prog) Stream.t =
  function
    (* Rule: sync |> skip, skip *)
  | Base Sync -> (Some (Phase []),[]) |> Streamutil.one
    (* Rule: ^P |> _|_, P *)
  | Base (Unsync u) -> (None, [Base u]) |> Streamutil.one
  | Cond (b, p) ->
    normalize p |>
    (* For each possible element *)
    Streamutil.map (fun (o, p) ->
      match o with
      (* Rule: ^P |> _|_, P *)
      | None -> Streamutil.one (None, [Cond (b,p)])
      | Some p' -> [
          (* Rule:
            P |> P1,P2
            ----------
            if (b) P |> (assert b;P1, assert b;P2)

          *)
          Some (Pre (b, p')), (Assert b)::p;
          (* Rule:
            P |> P1,P2
            ----------
            if (b) P |> _|_, assert !b

          *)
          None, [Assert (b_not b)]
        ]
        |> Stream.of_list
    ) |> Streamutil.concat
  | Loop ({range_var=x;range_lower_bound=lb;range_upper_bound=ub} as r, body) ->
      normalize body |>
      Streamutil.map (function
        | (Some p1, p2) ->
          let dec_ub = Bin (Minus, ub, Num 1) in
          let p1' = Pre (
            n_lt lb ub,
            ph_subst (x, lb) p1
          ) in
          let p2' = Assert (n_lt lb ub):: p_subst (x, dec_ub) p2 in
          let inc_var = Bin (Plus,Var x,Num 1) in
          let subbed_p1 = ph_subst (x, inc_var) p1 in
          let r' = { r with range_upper_bound = dec_ub } in
          [
            (* Rule:
              P |> P1, P2
              --------------------------------------------------------
              for x [n,m) {P} |> (assert (n<m);P1 {n/x}), assert (n<m)
              *)
            Some p1', [Assert (n_lt lb ub)];
            (* Rule:
              P |> P1, P2           P2' = assert (n<m);P2{m-1/x}
              ---------------------------------------------------
              for x [n,m) {P} |> for [n,m-1) {P2;P1 {x+1/x}}, P2'
             *)
            Some (make_global r' (prepend p2 subbed_p1)), p2';
            (* Rule:
              P |> P1, P2
              ---------------------------------------
              for x [n,m) {P} |> _|_, assert (n >= m)
             *)
            None, [Assert (n_ge lb ub)]
          ] |> Stream.of_list
          (* Rule: ^P |> _|_, P *)
        | (None, u) -> (None, [make_local r u]) |> Streamutil.one
      ) |> Streamutil.concat
(* Typesafe normalization of programs *)
and normalize: Proto.prog -> (p_phase option * p_prog) Stream.t =
  function
  | [] -> (None, []) |> Streamutil.one
  | x::xs ->
    Streamutil.product (normalize1 x) (normalize xs)
    |> Streamutil.map (fun (x,y) -> seq x y)
    |> Streamutil.concat

let make_phases ((o,p):p_phase option * p_prog) : p_phase list =
  let tl:(p_phase list) = match o with
  | None -> []
  | Some b -> [b]
  in
  Phase p::tl

(* Takes a program with Syncs and generates a program with phased blocks *)
let prog_to_s_prog (s:Proto.prog) : p_phase Stream.t =
  normalize s
  |> Streamutil.map (fun p -> make_phases p |> Stream.of_list)
  |> Streamutil.concat

(* Inline a set of variables as decls *)


let inline_globals (vars:VarSet.t) (p:'a phase) : 'a phase =
  VarSet.elements vars
  |> List.fold_left (fun p x ->
    Global (x, p)
  ) p


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

let p_phase_ser (p: p_phase) : Sexplib.Sexp.t =
  phase_ser (fun x -> prog_ser acc_inst_ser x |> s_list) p |> s_list

let kernel_ser (k:p_kernel) : Sexplib.Sexp.t =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    var_set_ser "locations" k.p_kernel_locations;
    unop "code" (p_phase_ser k.p_kernel_code);
  ]

let print_kernels (ks : p_kernel Stream.t) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Stream.iter (fun (k:p_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    kernel_ser k |> s_print
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