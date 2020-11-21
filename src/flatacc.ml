open Locsplit
open Exp
type cond_access = {
  ca_location: Sourceloc.location;
  ca_access: access;
  ca_cond: bexp;
}

let add_cond (b:bexp) (c:cond_access) : cond_access =
  { c with ca_cond = b_and c.ca_cond b }

type h_prog = {
  prog_locals: VarSet.t;
  prog_accesses: cond_access list;
}

type h_phase = h_prog Phasesplit.phase

type h_kernel = h_phase loc_kernel

type f_kernel = {
  f_kernel_local_variables: VarSet.t;
  f_kernel_accesses: cond_access list;
  f_kernel_pre: bexp;
  f_kernel_location: variable;
}

let l_kernel_to_h_kernel (k:l2_kernel) : f_kernel =
  let open Phasealign in
  let rec flatten_i (b:bexp) (i:u_inst) : cond_access list =
    match i with
    | UAcc (x, e) -> [{ca_location = x.var_loc; ca_access = e; ca_cond = b}]
    | UCond (b', p) -> flatten_p (b_and b' b) p
    | ULoop (r, p) -> flatten_p (b_and (range_to_cond r) b) p
  and flatten_p (b:bexp) (p:u_prog) : cond_access list =
    List.map (flatten_i b) p |> List.flatten
  in
  let rec loop_vars_i (i:u_inst) (vars:VarSet.t) : VarSet.t =
    match i with
    | UAcc _ -> vars
    | UCond (_, p) -> loop_vars_p p vars
    | ULoop (r, p) -> loop_vars_p p (VarSet.add r.range_var vars)
  and loop_vars_p (p:u_prog) (vars:VarSet.t) : VarSet.t =
    List.fold_right loop_vars_i p vars
  in
  {
    f_kernel_location = k.l_kernel_location;
    f_kernel_accesses = flatten_i (Bool true) k.l_kernel_code;
    f_kernel_local_variables = loop_vars_i k.l_kernel_code k.l_kernel_local_variables;
    f_kernel_pre = b_and_ex (List.map range_to_cond k.l_kernel_ranges);
  }

let translate2 (stream:l2_kernel Streamutil.stream) : f_kernel Streamutil.stream =
  let open Streamutil in
  map l_kernel_to_h_kernel stream

(* Converts a program into an h_program:
  1. we must make all loop variables unique
  2. we can then safely elide loop declarations
  3. we flatten conditions in accesses

  *)
let p_prog_to_h_prog (known:VarSet.t) (p:l_prog) : h_prog =
  (*
    Inline the conditions in each access.
    Ignores asserts because these must have been handled elsewhere.
    Assumes all variables are distinct. *)
  let flatten (p:l_prog) : cond_access list =
    let rec flatten_inst (b:bexp) (i: l_inst) : cond_access list =
      match i with
      | Base e -> [{ca_location = e.la_location; ca_access = e.la_access; ca_cond = b}]
      | Assert b -> []
      | Cond (b', l) -> flatten_prog (b_and b b') l
      | Local (y, l) -> flatten_prog b l
    and flatten_prog (b:bexp) (p:l_prog): cond_access list =
      List.map (flatten_inst b) p |> List.flatten
    in
    flatten_prog (Bool true) p
  in
  (* Get the asserts in the code *)
  let get_asserts (p: l_prog): bexp =
    let rec get_asserts_inst : l_inst -> bexp option =
      function
      | Base _ -> None
      | Assert b -> Some b
      | Cond (b, p) ->
        begin match get_asserts_prog p with
        | Some b' -> Some (b_impl b b')
        | None -> None
        end
      | Local (_, p) -> get_asserts_prog p
    and get_asserts_prog (p:l_prog) : bexp option =
      match Common.map_opt get_asserts_inst p with
      | [] -> None
      | l -> Some (b_and_ex l)
    in
    match get_asserts_prog p with
    | Some b -> b
    | None -> Bool true
  in
  (* Gets all variables declared in this code.
    Assumes all variables are distinct. *)
  let get_variables (p: l_prog) : VarSet.t =
      let open Proto in
    let rec names_inst (vars:VarSet.t) (i:l_inst) : VarSet.t =
      match i with
      | Base _
      | Assert _-> vars
      | Cond (_, l) -> names_prog vars l
      | Local (y, l) -> names_prog (VarSet.add y vars) l
    and names_prog (vars:VarSet.t) (p:l_prog): VarSet.t =
      List.fold_left names_inst vars p
    in
    names_prog VarSet.empty p
  in
  let la_subst (s:Subst.SubstPair.t) (a:l_access) : l_access =
    {
      la_access = Subst.ReplacePair.a_subst s a.la_access;
      la_location = a.la_location;
    }
  in
  (* Make all variables in the program distinct *)
  let (p:l_prog) = Phasealign.var_uniq_prog la_subst known p in
  (* Retrieve all accesses found in the program *)
  let pre = get_asserts p in
  (* Add the assert-preconditions to every access *)
  let ps = flatten p |> List.map (add_cond pre) in
  { prog_locals = get_variables p; prog_accesses = ps}

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

let translate (stream:l_kernel Streamutil.stream) : h_kernel Streamutil.stream =
  let open Streamutil in
  map l_kernel_to_h_kernel stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : h_kernel Streamutil.stream) : unit =
  let cond_access_ser (c:cond_access) : Smtlib.sexp =
    Smtlib.List [
      Serialize.a_ser c.ca_access;
      Serialize.b_ser c.ca_cond;
    ]
  in
  let p_ser (p: h_prog) : Smtlib.sexp =
    let accs = List.map cond_access_ser p.prog_accesses |> Serialize.s_list in
    Smtlib.List [
      Serialize.var_set_ser "locals" p.prog_locals;
      Serialize.unop "accesses" accs;
    ]
  in
  let ph_ser (ph:h_phase) : Smtlib.sexp =
    Phasesplit.phase_ser p_ser ph |> Serialize.s_list
  in
  print_loc_kernels ph_ser "flat-accesses" ks

let f_kernel_to_s (k:f_kernel) : Serialize.PPrint.t list =
  let open Serialize in
  let open PPrint in
  let acc_val_to_s a : string =
    mode_to_s a.access_mode ^ index_to_s a.access_index
  in
  let acc_to_s (a:cond_access) : t =
    Line (acc_val_to_s a.ca_access ^ " if " ^ b_to_s a.ca_cond ^";")
  in
  [
      Line ("location: " ^ k.f_kernel_location.var_name ^ ";");
      Line ("locals: " ^ var_set_to_s k.f_kernel_local_variables ^ ";");
      Line ("pre: " ^ b_to_s k.f_kernel_pre ^ ";");
      Line "{";
      Block (List.map acc_to_s k.f_kernel_accesses);
      Line "}"
  ]

let print_kernels2 (ks : f_kernel Streamutil.stream) : unit =
  print_endline "; locsplit";
  let count = ref 0 in
  Streamutil.iter (fun (k:f_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; loc " ^ (string_of_int curr));
    Serialize.PPrint.print_doc (f_kernel_to_s k)
  ) ks;
  print_endline "; end of locsplit"
