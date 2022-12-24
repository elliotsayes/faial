open Stage0
open Protocols

open Proto

type type_error =
| DuplicateLocs of Variable.t list
| DuplicateVars of Variable.t list
| UndefinedLocs of Variable.t list
| UndefinedVars of Variable.t list

type err_t = string * Location.t option

let typecheck_kernel (k:prog kernel) : err_t list =
  let handle (ctr: Variable.t list -> type_error) (errs:type_error list) (l:Variable.Set.t) : type_error list =
    if not (Variable.Set.is_empty l)
    then (Variable.Set.elements l |> ctr)::errs
    else errs
  in
  let dup_vars (l:Variable.t list) ctr (errs: type_error list) : type_error list =
    let rec iter (s:Variable.Set.t) (l:Variable.t list) : Variable.Set.t =
      match l with
      | [] -> Variable.Set.empty
      | x::l ->
        let rest = iter (Variable.Set.remove x s) l in
        if Variable.Set.mem x s
        then rest
        else Variable.Set.add x rest
    in
    handle ctr errs (iter (Variable.Set.of_list l) l)
  in
  let undef_vars (vars:Variable.Set.t) (p:prog) (errs: type_error list) : type_error list =
    Variable.Set.diff (Freenames.free_names_proto p Variable.Set.empty) vars
      |> handle (fun l -> UndefinedVars l) errs
  in
  let undef_locs (locs:Variable.Set.t) (p:prog) (errs:type_error list) : type_error list =
    Variable.Set.diff (Freenames.free_locs_proto p locs) locs
      |> handle (fun l -> UndefinedLocs l) errs
  in
  let errs : type_error list = [] in
  let all_vars : Variable.t list = List.append (k.kernel_local_variables |> Variable.Set.elements) (k.kernel_global_variables |> Variable.Set.elements) in
  let errs = dup_vars (k.kernel_arrays |> Variable.MapSetUtil.map_to_set |> Variable.Set.elements) (fun l -> DuplicateLocs l) errs in
  let errs = dup_vars all_vars (fun l -> DuplicateVars l) errs in
  let errs = undef_vars (Variable.Set.of_list all_vars) k.kernel_code errs in
  let errs = undef_locs (k.kernel_arrays |> Variable.MapSetUtil.map_to_set) k.kernel_code errs in
  let on_msg msg =
    List.map (fun x -> (msg ^ " '" ^ Variable.name x ^ "'", Variable.location_opt x))
  in
  errs |>
  List.concat_map (fun es ->
    match es with
    | DuplicateLocs l -> on_msg "Duplicate location" l
    | DuplicateVars l -> on_msg "Duplicate variable" l
    | UndefinedLocs l -> on_msg "Undefined location" l
    | UndefinedVars l -> on_msg "Undefined variable" l
  )
