open Phasealign
open Exp
type l_inst = access inst

type l_prog = access prog

type l_phase = l_prog phase

type 'a loc_kernel = {
  (* The shared location that can be accessed in the kernel. *)
  loc_kernel_location: variable;
  (* The code of a kernel performs the actual memory accesses. *)
  loc_kernel_code: 'a
}

type l_kernel = l_phase loc_kernel

(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

let rec filter_loc_inst (x:variable) (i:p_inst) : l_inst option =
  match i with
  | Base (Goal b) -> None
  | Base (Acc (y, e)) ->
    begin if var_equal x y then
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

and filter_loc_prog (x:variable) (l:p_prog) : l_prog option =
  match Common.map_opt (filter_loc_inst x) l with
  | [] -> None
  | l -> Some l

let rec filter_loc_phase (x:variable) : p_phase -> l_phase option =
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
        loc_kernel_location = x;
        loc_kernel_code = p;
      }
    | None -> None
  )

let translate (stream:p_kernel Stream.t) : l_kernel Stream.t =
  let open Streamutil in
  stream
  |> map (fun x ->
    p_kernel_to_l_kernel_list x |> Stream.of_list
  )
  |> concat

(* ------------------- SERIALIZE ---------------------- *)

let loc_kernel_ser (f:'a -> Sexplib.Sexp.t) (k: 'a loc_kernel) =
  let open Sexplib in
  Sexp.List [
    Sexp.Atom "kernel";
    Serialize.unop "location" (Sexp.Atom k.loc_kernel_location.var_name);
    Serialize.unop "code" (f k.loc_kernel_code);
  ]

let print_loc_kernels (f:'a -> Sexplib.Sexp.t) (lbl:string) (ks : 'a loc_kernel Stream.t) : unit =
  let open Serialize in
  print_endline ("; " ^ lbl);
  let count = ref 0 in
  Stream.iter (fun x ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    loc_kernel_ser f x |> s_print
  ) ks;
  print_endline ("; end of " ^ lbl)

let print_kernels (ks : l_kernel Stream.t) : unit =
  let open Sexplib in
  let p_ser (p:l_prog) : Sexp.t =
    prog_ser Serialize.a_ser p |> Serialize.s_list
  in
  let ph_ser (ph: l_phase) : Sexp.t =
    phase_ser p_ser ph |> Serialize.s_list
  in
  print_loc_kernels ph_ser "locsplit" ks
