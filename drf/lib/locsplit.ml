(*
 Given a phase-split kernel, generate a location-split kernel.
 *)
open Stage0
open Protocols

open Wellformed
open Phasesplit

module Kernel = struct
  type t = {
    (* The kernel name *)
    name : string;
    (* The shared locations that can be accessed in the kernel. *)
    array_name: string;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Variable.Set.t;
    (* Global ranges *)
    ranges: Range.t list;
    (* The code of a kernel performs the actual memory accesses. *)
    code: u_inst;
  }

  let to_s (k:t) : Indent.t list =
    let open Indent in
    let ranges =
      List.map Range.to_string k.ranges
      |> Common.join "; "
    in
    [
        Line ("array: " ^ k.array_name ^ ";");
        Line ("locals: " ^ Variable.set_to_string k.local_variables ^ ";");
        Line ("ranges: " ^ ranges ^ ";");
        Line "{";
        Block (u_inst_to_s k.code);
        Line "}"
    ]
  (* ------------ filter by location -------------- *)

  type 'a possibility =
    | Has of 'a
    | Might of 'a
    | Nothing

  let possibility_to_option =
    function
    | Has p -> Some p
    | Might p -> Some p
    | Nothing -> None

  let possibility_unwrap =
    function
    | Has p -> p
    | Might p -> p
    | Nothing -> failwith "possibility_get Nothing"

  let is_has =
    function
    | Has _ -> true
    | _ -> false

  (* Given an input phase, returns a phase with only accesses x.
    Changes the accesses to not contain the location info. *)
  let filter_by_location (x:Variable.t) (i: u_inst) : u_inst option =
    (* Filter programs *)
    let rec filter_i (i:u_inst) : u_inst possibility =
      match i with
      | UAssert b -> Might (UAssert b)
      | UAcc (y, _) -> if Variable.equal x y then Has i else Nothing
      | UCond (b, p) -> begin match filter_p p with
          | Some p -> Has (UCond (b, p))
          | None -> Nothing
        end
      | ULoop (r, p) -> begin match filter_p p with
          | Some p -> Has (ULoop (r, p))
          | None -> Nothing
        end
    and filter_p (p:u_prog) : u_prog option =
      let p = List.map filter_i p in
      if List.exists is_has p then
        Some (List.filter_map possibility_to_option p)
      else
        None
    in
    match filter_i i with
    | Has p -> Some p
    | _ -> None

  let from_phased (k:u_kernel) : t Streamutil.stream =
    Variable.Set.elements k.u_kernel_arrays
    |> Streamutil.from_list
    |> Streamutil.filter_map (fun x ->
      (* For every location *)
      match filter_by_location x k.u_kernel_code with
      | Some p ->
        (* Filter out code that does not touch location x *)
        Some {
          array_name = Variable.name x;
          name = k.u_kernel_name;
          ranges = k.u_kernel_ranges;
          local_variables = k.u_kernel_local_variables;
          code = p;
        }
      | None -> None (* No locations being used, so ignore *)
    )
end
(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

let translate (stream:u_kernel Streamutil.stream) : Kernel.t Streamutil.stream =
  stream
  |> Streamutil.map Kernel.from_phased
  (* We have a stream of streams, flatten it *)
  |> Streamutil.concat

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; locsplit";
  let count = ref 0 in
  Streamutil.iter (fun (k:Kernel.t) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; loc " ^ (string_of_int curr));
    Indent.print (Kernel.to_s k)
  ) ks;
  print_endline "; end of locsplit"
