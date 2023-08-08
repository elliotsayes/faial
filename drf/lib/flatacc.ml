(*
 Given a location-split kernel, generate a flat kernel.

 A flat-kernel has the following characteristics:
 - free from control-flow structures
 - all binders are hoisted
 - all concurrent accesses are available
 *)
open Stage0
open Protocols

open Wellformed
open Locsplit
open Exp

module CondAccess = struct
  type t = {
    location: Location.t;
    access: Access.t;
    cond: bexp;
  }

  let location (x:t) : Location.t = x.location

  let add (b:bexp) (c:t) : t =
    { c with cond = b_and c.cond b }

  let to_s (a:t) : Indent.t =
    let lineno = (Location.line a.location |> Index.to_base1 |> string_of_int) ^ ": " in
    Line (lineno ^ Access.to_string a.access ^ " if " ^
      b_to_string a.cond ^";")

end

module Kernel = struct
  type t = {
    name: string;
    array: string;
    local_variables: Variable.Set.t;
    accesses: CondAccess.t list;
    pre: bexp;
  }

  let to_s (k:t) : Indent.t list =
    [
        Line ("array: " ^ k.array ^ ";");
        Line ("locals: " ^ Variable.set_to_string k.local_variables ^ ";");
        Line ("pre: " ^ b_to_string k.pre ^ ";");
        Line "{";
        Block (List.map CondAccess.to_s k.accesses);
        Line "}"
    ]

  let from_loc_split (k:l2_kernel) : t =
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
    let rec flatten_i (b:bexp) (i:u_inst) : CondAccess.t list =
      match i with
      | UAssert _ -> failwith "Internall error: call rm_asserts first!"
      | UAcc (x, e) -> [CondAccess.{location = Variable.location x; access = e; cond = b}]
      | UCond (b', p) -> flatten_p (b_and b' b) p
      | ULoop (r, p) -> flatten_p (b_and (Range.to_cond r) b) p
    and flatten_p (b:bexp) (p:u_prog) : CondAccess.t list =
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
      name = k.l_kernel_name;
      array = k.l_kernel_array;
      accesses = flatten_p (Bool true) code;
      local_variables = loop_vars_i k.l_kernel_code k.l_kernel_local_variables;
      pre = b_and_ex (List.map Range.to_cond k.l_kernel_ranges);
    }
end


let translate (stream:l2_kernel Streamutil.stream) : Kernel.t Streamutil.stream =
  let open Streamutil in
  map Kernel.from_loc_split stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; flatacc";
  let count = ref 0 in
  Streamutil.iter (fun (k:Kernel.t) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; acc " ^ (string_of_int curr));
    Indent.print (Kernel.to_s k)
  ) ks;
  print_endline "; end of flatacc"
