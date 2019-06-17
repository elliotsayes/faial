open Proto
open Common

type type_error =
| DuplicateLocs of variable list
| DuplicateVars of variable list
| UndefinedLocs of variable list
| UndefinedVars of variable list

let typecheck_kernel (k:kernel) =
  let handle ctr errs l =
    if not (VarSet.is_empty l)
    then (VarSet.elements l |> ctr)::errs
    else errs
  in
  let dup_vars (l:variable list) ctr errs =
    let rec iter (s:VarSet.t) (l:variable list) =
      match l with
      | [] -> VarSet.empty
      | x::l ->
        let rest = iter (VarSet.remove x s) l in
        if VarSet.mem x s
        then rest
        else VarSet.add x rest
    in
    handle ctr errs (iter (VarSet.of_list l) l)
  in
  let undef_vars (vars:VarSet.t) (p:proto) errs : type_error list =
    VarSet.diff (Freenames.free_names_proto p VarSet.empty) vars
      |> handle (fun l -> UndefinedVars l) errs
  in
  let undef_locs (locs:VarSet.t) (p:proto) errs =
    VarSet.diff (Freenames.free_locs_proto p locs) locs
      |> handle (fun l -> UndefinedLocs l) errs
  in
  let errs = [] in
  let all_vars : variable list = List.append (k.kernel_local_variables |> VarSet.elements) (k.kernel_global_variables |> VarSet.elements) in
  let errs = dup_vars (k.kernel_locations |> VarSet.elements) (fun l -> DuplicateLocs l) errs in
  let errs = dup_vars all_vars (fun l -> DuplicateVars l) errs in
  let errs = undef_vars (VarSet.of_list all_vars) k.kernel_code errs in
  let errs = undef_locs k.kernel_locations k.kernel_code errs in
  errs
