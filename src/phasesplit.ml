open Proto
open Streamutil
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
  | Phase of 'a prog
  | Pre of bexp * 'a phase
  | Global of variable * 'a phase

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

(* ---------------- Translation-specific code ------------------- *)

type p_inst = acc_inst inst
type p_prog = acc_inst prog
type p_phase = acc_inst phase

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

let rec inst_to_phase (locals:VarSet.t) : s_inst -> p_phase Stream.t =
  function
  | Base p ->
    let (p:p_prog) = u_prog_to_p_prog p |> inline_locals locals in
    Stream.of_list [Phase p]
  | Loop (r, l) ->
    (* Create a loop per instruction in its body *)
    prog_to_phase locals l
    |> stream_map (make_global r)
  | Cond (b, l) ->
    (* Create a conditional per instruction in its body *)
    prog_to_phase locals l
    |> stream_map (fun p -> Pre (b, p))

and prog_to_phase (locals:VarSet.t) (l: s_prog) : p_phase Stream.t =
  List.fold_left
    (fun s i -> inst_to_phase locals i |> stream_seq s)
    (stream_make None)
    l

type p_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  p_kernel_locations: VarSet.t;
  (* The code of a kernel performs the actual memory accesses. *)
  p_kernel_code: p_phase;
}

let translate (k : s_prog kernel) : p_kernel Stream.t  =
  Streamutil.stream_map (fun p ->
      {
        p_kernel_locations = k.kernel_locations;
        p_kernel_code = inline_globals k.kernel_global_variables p
      }
    )
    (prog_to_phase k.kernel_local_variables k.kernel_code)

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
  | Phase p -> [prog_ser f p |> call "phase"]
  | Pre (b, p) -> unop "pre" (b_ser b) :: (phase_ser f p)
  | Global (x, p) ->
    let open Sexplib in
    unop "global" (Sexp.Atom x.var_name) :: (phase_ser f p)
(*
let p_prog_ser : p_prog -> Sexplib.Sexp.t =
  prog_ser acc_inst_ser
*)
let p_phase_ser (p: p_phase) : Sexplib.Sexp.t =
  phase_ser acc_inst_ser p |> s_list

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
