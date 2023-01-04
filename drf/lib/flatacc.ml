open Stage0
open Protocols

open Wellformed
open Locsplit
open Exp

type cond_access = {
  ca_location: Location.t;
  ca_access: Access.t;
  ca_cond: bexp;
}

let add_cond (b:bexp) (c:cond_access) : cond_access =
  { c with ca_cond = b_and c.ca_cond b }

type f_kernel = {
  f_kernel_name: string;
  f_kernel_array: string;
  f_kernel_local_variables: Variable.Set.t;
  f_kernel_accesses: cond_access list;
  f_kernel_pre: bexp;
}

let l_kernel_to_h_kernel (k:l2_kernel) : f_kernel =
  let is_assert (i:u_inst) =
    match i with
    | UAssert _ -> true
    | _ -> false
  in
  let is_not_assert i = not (is_assert i) in
  let rec has_asserts (p:u_prog) : bool =
    match p with
    | i :: p ->
      if is_assert i || has_asserts p then true
      else
      has_asserts (match i with
        | UAcc _
        | UAssert _ -> []
        | ULoop (_, p)
        | UCond (_, p) -> p
      )
    | [] -> false
  in
  let rm_asserts (p:u_prog) : u_prog =
    let rm_asserts_0 (p:u_prog) : u_prog =
      let asserts = List.filter_map (fun i ->
        match i with
        | UAssert b -> Some b
        | _ -> None
      ) p
      in
      if Common.list_is_empty asserts then
        p
      else
        [UCond (b_and_ex asserts, List.filter is_not_assert p)]
    in
    let rec rm_asserts_1 (p:u_prog) : u_prog =
      match p with
      | i ::l ->
        let i = match i with
        | UCond (b, p) -> UCond (b, rm_asserts_0 (rm_asserts_1 p))
        | ULoop (r, p) -> ULoop (r, rm_asserts_0 (rm_asserts_1 p))
        | i -> i
        in
        i :: rm_asserts_1 l
      | [] -> []
    in
    rm_asserts_1 p |> rm_asserts_0
  in
  let rec flatten_i (b:bexp) (i:u_inst) : cond_access list =
    match i with
    | UAssert _ -> failwith "Internall error: call rm_asserts first!"
    | UAcc (x, e) -> [{ca_location = Variable.location x; ca_access = e; ca_cond = b}]
    | UCond (b', p) -> flatten_p (b_and b' b) p
    | ULoop (r, p) -> flatten_p (b_and (Range.to_cond r) b) p
  and flatten_p (b:bexp) (p:u_prog) : cond_access list =
    List.map (flatten_i b) p |> List.flatten
  in
  let rec loop_vars_i (i:u_inst) (vars:Variable.Set.t) : Variable.Set.t =
    match i with
    | UAssert _
    | UAcc _ -> vars
    | UCond (_, p) -> loop_vars_p p vars
    | ULoop (r, p) -> loop_vars_p p (Variable.Set.add r.var vars)
  and loop_vars_p (p:u_prog) (vars:Variable.Set.t) : Variable.Set.t =
    List.fold_right loop_vars_i p vars
  in
  let code =
    if has_asserts [k.l_kernel_code]
    then [k.l_kernel_code] |> rm_asserts
    else [k.l_kernel_code]
  in
  {
    f_kernel_name = k.l_kernel_name;
    f_kernel_array = k.l_kernel_array;
    f_kernel_accesses = flatten_p (Bool true) code;
    f_kernel_local_variables = loop_vars_i k.l_kernel_code k.l_kernel_local_variables;
    f_kernel_pre = b_and_ex (List.map Range.to_cond k.l_kernel_ranges);
  }

let translate (stream:l2_kernel Streamutil.stream) : f_kernel Streamutil.stream =
  let open Streamutil in
  map l_kernel_to_h_kernel stream

(* ------------------- SERIALIZE ---------------------- *)

let f_kernel_to_s (k:f_kernel) : Indent.t list =
  let open Indent in
  let acc_to_s (a:cond_access) : t =
    let lineno = (Location.line a.ca_location |> Index.to_base1 |> string_of_int) ^ ": " in
    Line (lineno ^ Access.to_string a.ca_access ^ " if " ^
      b_to_string a.ca_cond ^";")
  in
  [
      Line ("array: " ^ k.f_kernel_array ^ ";");
      Line ("locals: " ^ Variable.set_to_string k.f_kernel_local_variables ^ ";");
      Line ("pre: " ^ b_to_string k.f_kernel_pre ^ ";");
      Line "{";
      Block (List.map acc_to_s k.f_kernel_accesses);
      Line "}"
  ]

let print_kernels (ks : f_kernel Streamutil.stream) : unit =
  print_endline "; flatacc";
  let count = ref 0 in
  Streamutil.iter (fun (k:f_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; acc " ^ (string_of_int curr));
    Indent.print (f_kernel_to_s k)
  ) ks;
  print_endline "; end of flatacc"
