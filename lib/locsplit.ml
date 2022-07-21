open Wellformed
open Phasesplit
open Exp

type l2_kernel = {
  (* The kernel name *)
  l_kernel_name : string;
  (* The shared locations that can be accessed in the kernel. *)
  l_kernel_array: string;
  (* The internal variables are used in the code of the kernel.  *)
  l_kernel_local_variables: VarSet.t;
  (* Global ranges *)
  l_kernel_ranges: range list;
  (* The code of a kernel performs the actual memory accesses. *)
  l_kernel_code: u_inst;
  (* A thread-local pre-condition that is true on all phases. *)
}

(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

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
let filter_by_location (x:variable) (i: u_inst) : u_inst option =
  (* Filter programs *)
  let rec filter_i (i:u_inst) : u_inst possibility =
    match i with
    | UAssert b -> Might (UAssert b)
    | UAcc (y, _) -> if var_equal x y then Has i else Nothing
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
      Some (Common.map_opt possibility_to_option p)
    else
      None
  in
  match filter_i i with
  | Has p -> Some p
  | _ -> None

let translate (stream:u_kernel Streamutil.stream) : l2_kernel Streamutil.stream =
  let open Streamutil in
  stream
  |> map (fun k ->
    (* For every kernel *)
    VarSet.elements k.u_kernel_arrays
    |> from_list
    |> map_opt (fun x ->
      (* For every location *)
      match filter_by_location x k.u_kernel_code with
      | Some p ->
        (* Filter out code that does not touch location x *)
        Some {
          l_kernel_array = var_name x;
          l_kernel_name = k.u_kernel_name;
          l_kernel_ranges = k.u_kernel_ranges;
          l_kernel_local_variables = k.u_kernel_local_variables;
          l_kernel_code = p;
        }
      | None -> None (* No locations being used, so ignore *)
    )
  )
  (* We have a stream of streams, flatten it *)
  |> concat

(* ------------------- SERIALIZE ---------------------- *)

let l_kernel_to_s (k:l2_kernel) : Serialize.PPrint.t list =
  let open Serialize.PPrint in
  let ranges =
    List.map r_to_s k.l_kernel_ranges
    |> Common.join "; "
  in
  [
      Line ("array: " ^ k.l_kernel_array ^ ";");
      Line ("locals: " ^ var_set_to_s k.l_kernel_local_variables ^ ";");
      Line ("ranges: " ^ ranges ^ ";");
      Line "{";
      Block (u_inst_to_s k.l_kernel_code);
      Line "}"
  ]

let print_kernels (ks : l2_kernel Streamutil.stream) : unit =
  print_endline "; locsplit";
  let count = ref 0 in
  Streamutil.iter (fun (k:l2_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; loc " ^ (string_of_int curr));
    Serialize.PPrint.print_doc (l_kernel_to_s k)
  ) ks;
  print_endline "; end of locsplit"
