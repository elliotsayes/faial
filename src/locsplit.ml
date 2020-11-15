open Phasealign
open Phasesplit
open Exp
type l_access = {
  la_access: access;
  la_location: Sourceloc.location;
}

type l_inst = l_access inst

type l_prog = l_access prog

type l_phase = l_prog phase

type 'a loc_kernel = {
  (* The shared location that can be accessed in the kernel. *)
  loc_kernel_location: variable;
  (* The code of a kernel performs the actual memory accesses. *)
  loc_kernel_code: 'a
}

type l_kernel = l_phase loc_kernel

(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)


(* Given an input phase, returns a phase with only accesses x.
   Changes the accesses to not contain the location info. *)
let filter_loc (x:variable) (p: p_phase) : l_phase option =
  (* Filter programs *)
  let filter_prog (l:p_prog) : l_prog option =
    (* To ensure correctness, we must preserve asserts, so we must always
        return them. However, because we may end up with a program that only
        has asserts and no accesses, we must then check if there is at least
        one accesses in the returned program. *)
    let rec filter_inst (i:p_inst) : l_inst option =
      match i with
      | Base (y, e) ->
        begin if var_equal x y then
          Some (Base {la_location = x.var_loc; la_access = e})
        else
          None
        end
      | Assert b -> Some (Assert b)
      | Cond (b, l) ->
        begin match filter_prog l with
          | Some l -> Some (Cond (b, l))
          | None -> None
        end
      | Local (y, l) ->
        begin match filter_prog l with
          | Some l -> Some (Local (y, l))
          | None -> None
        end

    and filter_prog l : l_prog option =
      match Common.map_opt filter_inst l with
      | [] -> None
      | l -> Some l
    in
    let rec has_loc_p (p: l_prog) : bool =
      List.exists has_loc_i p
    and  has_loc_i (i:l_inst) : bool =
      match i with
      | Base _ -> true
      | Assert _ -> false
      | Cond (_, p) -> has_loc_p p
      | Local (_, p) -> has_loc_p p
    in
    match filter_prog l with
    (* If there is some program, we must then check if there is at least one
      access in the program, so that we can return it, otherwise ignore it. *)
    | Some p -> if has_loc_p p then Some p else None
    | None -> None
  in
  (* Filter phases *)
  let rec filter_phase: p_phase -> l_phase option =
  function
  | Phase p ->
    begin match filter_prog p with
    | Some p -> Some (Phase p)
    | None -> None
    end
  | Pre (b,p) ->
    begin match filter_phase p with
    | Some p -> Some (Pre (b, p))
    | None -> None
    end
  | Global (y, p) ->
    begin match filter_phase p with
    | Some p -> Some (Global (y, p))
    | None -> None
    end
  in
  (* run the whole thing *)
  filter_phase p

let p_kernel_to_l_kernel_list (k:p_kernel) : l_kernel list =
  let open Proto in
  VarSet.elements k.p_kernel_locations
  |> Common.map_opt (fun x ->
    match filter_loc x k.p_kernel_code with
    | Some p -> Some {
        loc_kernel_location = x;
        loc_kernel_code = p;
      }
    | None -> None
  )

let translate (stream:p_kernel Streamutil.stream) : l_kernel Streamutil.stream =
  let open Streamutil in
  stream
  |> map (fun x ->
    p_kernel_to_l_kernel_list x |> Streamutil.from_list
  )
  |> concat

(* ------------------- SERIALIZE ---------------------- *)

let loc_kernel_ser (f:'a -> Smtlib.sexp) (k: 'a loc_kernel) =
  let open Smtlib in
  List [
    Atom (Symbol "kernel");
    Serialize.unop "location" (Atom (String k.loc_kernel_location.var_name));
    Serialize.unop "code" (f k.loc_kernel_code);
  ]

let print_loc_kernels (f:'a -> Smtlib.sexp) (lbl:string) (ks : 'a loc_kernel Streamutil.stream) : unit =
  let open Serialize in
  print_endline ("; " ^ lbl);
  let count = ref 0 in
  Streamutil.iter (fun x ->
    let curr = !count + 1 in
    count := curr;
    print_endline ("; phase " ^ (string_of_int curr));
    loc_kernel_ser f x |> s_print
  ) ks;
  print_endline ("; end of " ^ lbl)

let print_kernels (ks : l_kernel Streamutil.stream) : unit =
  let p_ser (p:l_prog) : Smtlib.sexp =
    prog_ser (fun a -> Serialize.a_ser a.la_access) p |> Serialize.s_list
  in
  let ph_ser (ph: l_phase) : Smtlib.sexp =
    phase_ser p_ser ph |> Serialize.s_list
  in
  print_loc_kernels ph_ser "locsplit" ks
