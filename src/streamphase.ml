open Proto
open Streamutil
(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

(* A synchronized-program has multiple goals to prove, so want to flatten
   each phased block into a list. Phased blocks may be nested inside a
   conditional or a variable declaration (loop), and in which case,
   we must ensure we preserve that structure. *)

let rec inst_to_phase_stream : 'a base_inst -> ('a phase) Stream.t =
  function
  | Base p -> Stream.of_list [Phase p]
  | Loop (r, l) ->
    prog_to_phase_stream l
    |> stream_map (fun p ->
      Global (r, p)
    )
  | Cond (b, l) ->
    prog_to_phase_stream l
    |> stream_map (fun p ->
      Pre (b, p)
    )

and prog_to_phase_stream (l: ('a base_inst) list) : ('a phase) Stream.t =
  List.fold_left
    (fun s i -> inst_to_phase_stream i |> stream_seq s)
    (stream_make None)
    l

let translate (k : s_prog kernel) : u_prog phase kernel Stream.t  =
  Streamutil.stream_map (fun p ->
      { k with kernel_code = p }
    )
    (prog_to_phase_stream k.kernel_code)

let print_kernels (ks : u_prog phase kernel Stream.t) : unit =
  let open Serialize in
  print_endline "; conc";
  let count = ref 0 in
  Stream.iter (fun x ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    kernel_ser u_phase_ser x |> s_print
  ) ks;
  print_endline "; end of conc"
