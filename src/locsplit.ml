open Phasesplit

type l_inst = Proto.access inst

type l_prog = Proto.access prog

type l_phase = l_prog phase

type l_kernel = {
  (* The shared location that can be accessed in the kernel. *)
  l_kernel_location: Proto.variable;
  (* The code of a kernel performs the actual memory accesses. *)
  l_kernel_code: l_phase
}


(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

let rec filter_loc_inst (x:Proto.variable) (i:p_inst) : l_inst option =
  match i with
  | Base (Goal b) -> None
  | Base (Acc (y, e)) ->
    begin if Proto.var_equal x y then
      Some (Base e)
    else
      None
    end
  | Cond (b, l) ->
    begin match filter_loc_prog x l with
      | Some l -> Some (Cond (b, l))
      | None -> None
    end
  | Local (y, l) ->
    begin match filter_loc_prog x l with
      | Some l -> Some (Local (y, l))
      | None -> None
    end

and filter_loc_prog (x:Proto.variable) (l:p_prog) : l_prog option =
  match Common.map_opt (filter_loc_inst x) l with
  | [] -> None
  | l -> Some l

let rec filter_loc_phase (x:Proto.variable) : p_phase -> l_phase option =
  function
  | Phase p ->
    begin match filter_loc_prog x p with
    | Some p -> Some (Phase p)
    | None -> None
    end
  | Pre (b,p) ->
    begin match filter_loc_phase x p with
    | Some p -> Some (Pre (b, p))
    | None -> None
    end
  | Global (y, p) ->
    begin match filter_loc_phase x p with
    | Some p -> Some (Global (y, p))
    | None -> None
    end


let p_kernel_to_l_kernel_list (k:p_kernel) : l_kernel list =
  let open Proto in
  VarSet.elements k.p_kernel_locations
  |> Common.map_opt (fun x ->
    match filter_loc_phase x k.p_kernel_code with
    | Some p -> Some {
        l_kernel_location = x;
        l_kernel_code = p;
      }
    | None -> None
  )

let translate (stream:p_kernel Stream.t) : l_kernel Stream.t =
  let open Streamutil in
  stream
  |> stream_map (fun x ->
    p_kernel_to_l_kernel_list x |> Stream.of_list
  )
  |> stream_concat

(* ------------------- SERIALIZE ---------------------- *)

let l_kernel_ser (k: l_kernel) =
  let open Sexplib in
  let p_ser : l_prog -> Sexp.t =
    (fun x -> prog_ser Serialize.a_ser x |> Serialize.s_list)
  in
  Sexp.List [
    Sexp.Atom "kernel";
    Serialize.unop "location" (Sexp.Atom k.l_kernel_location.var_name);
    Serialize.unop "code" (phase_ser p_ser k.l_kernel_code |> Serialize.s_list);
  ]

let print_kernels (ks : l_kernel Stream.t) : unit =
  let open Serialize in
  print_endline "; loc";
  let count = ref 0 in
  Stream.iter (fun x ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; location " ^ (string_of_int curr));
    l_kernel_ser x |> s_print
  ) ks;
  print_endline "; end of loc"
