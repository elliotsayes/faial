open Phasealign
open Phasesplit
open Exp

type l2_kernel = {
  (* The shared locations that can be accessed in the kernel. *)
  l_kernel_location: variable;
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

(* Given an input phase, returns a phase with only accesses x.
   Changes the accesses to not contain the location info. *)
let filter_by_location (x:variable) (i: u_inst) : u_inst option =
  let bind p f =
    match p with
    | Has p -> Has (f p)
    | Might p -> Might (f p)
    | Nothing -> Nothing
  in
  let map_has p f =
    match p with
    | Has p -> Has (f p)
    | _ -> Nothing
  in
  (* Filter programs *)
  let rec filter_i (i:u_inst) : u_inst possibility =
    match i with
    | UAcc (y, _) -> if var_equal x y then Has i else Nothing
    | UCond (b, p) -> map_has (filter_p p) (fun p -> UCond (b, p))
    | ULoop (r, p) -> map_has (filter_p p) (fun p -> ULoop (r, p))
  and filter_p (p:u_prog) : u_prog possibility =
    match p with
    | [] -> Nothing
    | i :: p ->
      begin match filter_i i, filter_p p with
      | Nothing, p
        -> p
      | i, Nothing
        -> bind i (fun i -> [i])
      | Has i, Has p
      | Has i, Might p
      | Might i, Has p
        -> Has (i::p)
      | Might i, Might p
        -> Might p
      end
  in
  match filter_i i with
  | Has p -> Some p
  | _ -> None

let translate2 (stream:u_kernel Streamutil.stream) : l2_kernel Streamutil.stream =
  let open Streamutil in
  stream
  |> map (fun k ->
    (* For every kernel *)
    VarSet.elements k.u_kernel_locations
    |> from_list
    |> map_opt (fun x ->
      (* For every location *)
      match filter_by_location x k.u_kernel_code with
      | Some p ->
        (* Filter out code that does not touch location x *)
        Some {
          l_kernel_location = x;
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
      Line ("location: " ^ k.l_kernel_location.var_name ^ ";");
      Line ("locals: " ^ var_set_to_s k.l_kernel_local_variables ^ ";");
      Line ("ranges: " ^ ranges ^ ";");
      Line "{";
      Block (u_inst_to_s k.l_kernel_code);
      Line "}"
  ]

let print_kernels2 (ks : l2_kernel Streamutil.stream) : unit =
  print_endline "; locsplit";
  let count = ref 0 in
  Streamutil.iter (fun (k:l2_kernel) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; loc " ^ (string_of_int curr));
    Serialize.PPrint.print_doc (l_kernel_to_s k)
  ) ks;
  print_endline "; end of locsplit"
