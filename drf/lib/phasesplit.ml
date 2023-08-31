open Stage0
open Protocols

open Exp
open Proto
open Common
open Streamutil

(* ---------------- SECOND STAGE OF TRANSLATION ---------------------- *)

module Phased = struct
  type t = {
    code: Unsync.t;
    ranges: Range.t list;
  }


  let add (r:Range.t) (bi:t) : t =
    { bi with ranges = r :: bi.ranges }

  (* Implements |> *)
  let from_aligned (pre:bexp) : Aligned.t -> t stream =
    let rec phase: Aligned.t -> t stream =
      function
        (* ^P; sync |> { P } *)
      | Sync u ->
        {
          code = Cond (pre, u);
          ranges = []
        }
        |> Streamutil.one
      | Loop (p, r, q) ->
        (* Rule:
          P |> p    q = { for x in [n,m) Q | Q \in p }
          ------------------------------
          for x in [n,m) {Q} |> q
        *)
        (* Break down the body into phases, and prefix each phase with the
            binding *)
        phase p
        |> Streamutil.sequence (
          phase q
          |> Streamutil.map (fun bi ->
            (* For every phase in q, prefix it with variable in r *)
            add r bi
          )
        )
      | Seq (p, q) ->
        (* Rule:
          P |> p      Q |> q
          ------------------
          P;Q |> p U q
          *)
        phase p |> Streamutil.sequence (phase q)
    in
    phase

end

exception PhasesplitException of (string * Location.t option) list

module Kernel = struct
  type t = {
    (* The kernel name *)
    name : string;
    (* The shared locations that can be accessed in the kernel. *)
    arrays: Variable.Set.t;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables: Variable.Set.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables: Variable.Set.t;
    (* Global ranges *)
    ranges: Range.t list;
    (* The code of a kernel performs the actual memory accesses. *)
    code: Unsync.t;
  }

  let from_aligned (k: Aligned.t Kernel.t) : t stream =
    let p_to_k ((bi,locations):(Phased.t * Variable.Set.t)) : t =
      (* Check for undefs *)
      (* 1. compute all globals *)
      let globals =
        List.map (fun r -> let open Range in r.var) bi.ranges
        |> Variable.Set.of_list
        |> Variable.Set.union k.global_variables
      in
      (* 2. compute all free names in the ranges *)
      let fns = List.fold_right Freenames.free_names_range bi.ranges Variable.Set.empty in
      (* 3. check if there are any locals *)
      let errs = Variable.Set.diff fns globals
        |> Variable.Set.elements
        |> List.map (fun (x:Variable.t) ->
          "Barrier divergence error: cannot use thread-local variable '" ^
          (Variable.name x) ^ "' in synchronized control flow",
          (Variable.location_opt x)
          )
      in
      if List.length errs > 0 then (
        prerr_endline (List.map fst errs |> String.concat "\n");
        raise (PhasesplitException errs)
      ) else
        {
          name = k.name;
          local_variables = k.local_variables;
          global_variables = k.global_variables;
          arrays = locations;
          ranges = bi.ranges;
          code = bi.code;
        }
    in
    Phased.from_aligned k.pre k.code
    |> filter_map (fun b ->
      (* Get locations of u_prog *)
      let locations = Unsync.write_locations b.Phased.code Variable.Set.empty in
      if Variable.Set.is_empty locations then None
      else Some (b, locations)
    )
    |> Streamutil.map p_to_k


  let to_s (k:t) : Indent.t list =
    let open Indent in
    let ranges =
      List.map Range.to_string k.ranges
      |> join "; "
    in
    [
        Line ("arrays: " ^ Variable.set_to_string k.arrays ^ ";");
        Line ("globals: " ^ Variable.set_to_string k.global_variables ^ ";");
        Line ("locals: " ^ Variable.set_to_string k.local_variables ^ ";");
        Line ("ranges: " ^ ranges ^ ";");
        Line "{";
        Block (Unsync.to_s k.code);
        Line "}"
    ]

end

let translate (ks: Aligned.t Proto.Kernel.t stream) (_:bool) : Kernel.t stream =
  map Kernel.from_aligned ks |> concat


(* ---------------------- SERIALIZATION ------------------------ *)


let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; conc";
  let count = ref 0 in
  Streamutil.iter (fun (k:Kernel.t) ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    Indent.print (Kernel.to_s k)
  ) ks;
  print_endline "; end of conc"
