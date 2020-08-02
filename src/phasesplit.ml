open Proto
open Streamutil
open Subst
(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

(*
  In this stage we do two simplifications:
  1. We simplify loops/decls to not have ranges by converting the
     range as conditionals.
  2. We create a phase per u_prog, so the result becomes a stream (list)
     of programs.
  *)

(* A local instruction/program *)

type 'a inst =
  | Base of 'a
  | Cond of bexp * ('a inst list)
  | Local of variable * ('a inst list)

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

(* Constructor that converts a decl+range into a decl+loop *)

let range_to_cond (r:range) =
  b_and
    (n_le r.range_lower_bound (Var r.range_var))
    (n_lt (Var r.range_var) r.range_upper_bound)

(* Helpful constructors that allow converting loops into global/local
   declarations *)

let make_local (r:range) (ls:'a prog) : 'a inst =
  Local (r.range_var, [Cond (range_to_cond r, ls)])

let make_global (r:range) (ls:'a phase) : 'a phase =
  Global (r.range_var, Pre (range_to_cond r, ls))

(* Inline a set of variables as decls *)

let inline_locals (vars:VarSet.t) (p:'a prog) : 'a prog =
  VarSet.elements vars
  |> List.fold_left (fun p x ->
    [Local (x, p)]
  ) p

let inline_globals (vars:VarSet.t) (p:'a phase) : 'a phase =
  VarSet.elements vars
  |> List.fold_left (fun p x ->
    Global (x, p)
  ) p

(* Variable substitution. *)

let prog_subst (f:SubstPair.t -> 'a -> 'a) (s:SubstPair.t) (p:'a prog) : 'a prog =
  let rec i_subst (s:SubstPair.t) (i:'a inst) : 'a inst =
    match i with
    | Base b -> Base (f s b)
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

(* Make variables distinct. *)

let normalize_prog (f:SubstPair.t -> 'a -> 'a) (known:Proto.VarSet.t) (p:'a prog) : 'a prog =
  let open Bindings in
  let rec norm_inst (i:'a inst) (xs:Proto.VarSet.t) : 'a inst * Proto.VarSet.t =
    match i with
    | Local (x, p) ->
      if VarSet.mem x xs then (
        let new_x : Proto.variable = generate_fresh_name x xs in
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
    | Base _ -> i, xs
  and norm_prog (p:'a prog) (xs:Proto.VarSet.t) : 'a prog * Proto.VarSet.t =
    match p with
    | [] -> ([], xs)
    | i::p ->
      let (i, xs) = norm_inst i xs in
      let (p, xs) = norm_prog p xs in
      (i::p, xs)
  in
  norm_prog p known |> fst

let normalize_phase (f:SubstPair.t -> 'a -> 'a) (known:Proto.VarSet.t) (p:'a phase) : 'a phase =
  let open Bindings in
  let rec norm (i:'a phase) (xs:Proto.VarSet.t) : 'a phase * Proto.VarSet.t =
    match i with
    | Global (x, p) ->
      if VarSet.mem x xs then (
        let new_x : Proto.variable = generate_fresh_name x xs in
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


(* ---------------- Translation-specific code ------------------- *)

type p_inst = acc_inst inst
type p_prog = acc_inst prog
type p_phase = p_prog phase

type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: p_phase;
}

(* Given an unsynchronized instruction, simplify the loops so that
   there are no ranges. *)

let rec u_inst_to_p_inst (i: u_inst) : p_inst =
  match i with
  | Proto.Base x -> Base x
  | Proto.Cond (b, ls) -> Cond (b, u_prog_to_p_prog ls)
  | Proto.Loop (r, ls) -> make_local r (u_prog_to_p_prog ls)
and u_prog_to_p_prog (ls: u_prog) : p_prog =
  List.map u_inst_to_p_inst ls

(* A synchronized-program has multiple goals to prove, so want to flatten
   each phased block into a list. Phased blocks may be nested inside a
   conditional or a variable declaration (loop), and in which case,
   we must ensure we preserve that structure. *)

let rec inst_to_phase (pre:bexp) (locals:VarSet.t) : s_inst -> p_phase Stream.t =
  function
  | Base p ->
    let (p:p_prog) = u_prog_to_p_prog p |> inline_locals locals in
    Stream.of_list [ Phase [Cond (pre,p)]]
  | Loop (r, l) ->
    (* Create a loop per instruction in its body *)
    prog_to_phase pre locals l
    |> stream_map (make_global r)
  | Cond (b, l) ->
    (* Create a conditional per instruction in its body *)
    prog_to_phase pre locals l
    |> stream_map (fun p -> Pre (b, p))

and prog_to_phase (pre:bexp) (locals:VarSet.t) (l: s_prog) : p_phase Stream.t =
  List.fold_left
    (fun s i -> inst_to_phase pre locals i |> stream_seq s)
    (stream_make None)
    l

let translate (k : s_prog kernel) : p_kernel Stream.t  =
  Streamutil.stream_map (fun p ->
      {
        p_kernel_locations = k.kernel_locations;
        p_kernel_code = inline_globals k.kernel_global_variables p
      }
    )
    (prog_to_phase k.kernel_pre k.kernel_local_variables k.kernel_code)

(* --------------------SERIALIZE -------------------------- *)

open Serialize

let rec inst_ser (f : 'a -> Sexplib.Sexp.t) : 'a inst -> Sexplib.Sexp.t =
  function
  | Base a -> f a
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
let p_prog_ser : p_prog -> Sexplib.Sexp.t =
  prog_ser acc_inst_ser
*)
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
