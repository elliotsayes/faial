open Locsplit
open Exp
type cond_access = access * bexp

type h_prog = {
  prog_locals: VarSet.t;
  prog_accesses: cond_access list;
}

type h_phase = h_prog Phasesplit.phase

type h_kernel = h_phase loc_kernel

(* Converts a program into an h_program:
  1. we must make all loop variables unique
  2. we can then safely elide loop declarations
  3. we flatten conditions in accesses

  *)
let p_prog_to_h_prog (known:VarSet.t) (p:l_prog) : h_prog =
  (* Inline the conditions in each access.
      Assumes all variables are distinct. *)
  let flatten (p:l_prog) : cond_access list =
    let open Proto in
    let rec flatten_inst (b:bexp) (i: l_inst) : cond_access list =
      match i with
      | Base e -> [(e, b)]
      | Cond (b', l) -> flatten_prog (b_and b b') l
      | Local (y, l) -> flatten_prog b l
    and flatten_prog (b:bexp) (p:l_prog): cond_access list =
      List.map (flatten_inst b) p |> List.flatten
    in
    flatten_prog (Bool true) p
  in

  (* Gets all variables declared in this code.
    Assumes all variables are distinct. *)
  let get_variables (p: l_prog) : VarSet.t =
      let open Proto in
    let rec names_inst (vars:VarSet.t) (i:l_inst) : VarSet.t =
      match i with
      | Base _ -> vars
      | Cond (_, l) -> names_prog vars l
      | Local (y, l) -> names_prog (VarSet.add y vars) l
    and names_prog (vars:VarSet.t) (p:l_prog): VarSet.t =
      List.fold_left names_inst vars p
    in
    names_prog VarSet.empty p
  in
  (* Make all variables in the program distinct *)
  let p = Phasesplit.normalize_prog Subst.ReplacePair.a_subst known p in

  { prog_locals = get_variables p; prog_accesses = flatten p}

let l_phase_to_h_phase (p:l_phase) : h_phase =
  let rec tr_phase (xs:VarSet.t) (p:l_phase) : h_phase =
    match p with
    | Phase p -> Phase (p_prog_to_h_prog xs p)
    | Pre (b, p) -> Pre (b, tr_phase xs p)
    | Global (x, p) -> Global (x, tr_phase (VarSet.add x xs) p)
  in
  tr_phase VarSet.empty p

let l_kernel_to_h_kernel (k:l_kernel) : h_kernel =
  { k with
    loc_kernel_code = l_phase_to_h_phase k.loc_kernel_code
  }

let translate (stream:l_kernel Stream.t) : h_kernel Stream.t =
  let open Streamutil in
  stream_map l_kernel_to_h_kernel stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : h_kernel Stream.t) : unit =
  let open Sexplib in
  let cond_access_ser ((e,b):cond_access) : Sexp.t =
    Sexp.List [
      Serialize.a_ser e;
      Serialize.b_ser b;
    ]
  in
  let p_ser (p: h_prog) : Sexp.t =
    let accs = List.map cond_access_ser p.prog_accesses |> Serialize.s_list in
    Sexp.List [
      Serialize.var_set_ser "locals" p.prog_locals;
      Serialize.unop "accesses" accs;
    ]
  in
  let ph_ser (ph:h_phase) : Sexp.t =
    Phasesplit.phase_ser p_ser ph |> Serialize.s_list
  in
  print_loc_kernels ph_ser "flat-accesses" ks
