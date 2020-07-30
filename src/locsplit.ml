open Proto
(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

let rec filter_loc_inst (x:variable) (i:u_inst) : l_inst option =
  match i with
  | Base (Goal b) -> None
  | Base (Acc (y, e)) ->
    begin
      if var_equal x y then
        Some (Base e)
      else
        None
    end
  | Cond (b, l) ->
    begin
      let l = filter_loc_prog x l in
      match l with
      | Some l -> Some (Cond (b, l))
      | None -> None
    end
  | Loop (r, l) ->
    begin
      let l = filter_loc_prog x l in
      match l with
      | Some l -> Some (Loop (r, l))
      | None -> None
    end
and filter_loc_prog (x:variable) (l:u_prog) : l_prog option =
  match Common.map_opt (filter_loc_inst x) l with
  | [] -> None
  | l -> Some l

let rec filter_loc_phase (x:variable) (p:u_prog phase) : l_prog phase option =
  match p with
  | Phase l ->
    begin match filter_loc_prog x l with
    | Some l -> Some (Phase l)
    | None -> None
    end
  | Pre (b, p) ->
    begin match filter_loc_phase x p with
    | Some p -> Some (Pre (b, p))
    | None -> None
    end
  | Global (r, p) ->
    begin match filter_loc_phase x p with
    | Some p -> Some (Global (r, p))
    | None -> None
    end

let p_kernel_to_l_kernel_list (k:u_prog phase kernel) : l_kernel list =
  VarSet.elements k.kernel_locations
  |> Common.map_opt (fun x ->
    match filter_loc_phase x k.kernel_code with
    | Some p -> Some {
        l_kernel_location = x;
        l_kernel_global_variables = k.kernel_global_variables;
        l_kernel_local_variables = k.kernel_local_variables;
        l_kernel_code = p;
      }
    | None -> None
  )

let translate (stream:u_prog phase kernel Stream.t) : l_kernel Stream.t =
  let open Streamutil in
  stream
  |> stream_map (fun x ->
    p_kernel_to_l_kernel_list x |> Stream.of_list
  )
  |> stream_concat

let print_kernels (ks : l_kernel Stream.t) : unit =
  let open Serialize in
  print_endline "; loc";
  let count = ref 0 in
  Stream.iter (fun x ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    l_kernel_ser x |> s_print
  ) ks;
  print_endline "; end of loc"
