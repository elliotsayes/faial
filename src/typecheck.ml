open Proto
open Common

type type_error =
| DuplicateLocs of string list
| DuplicateVars of string list
| UndefinedLocs of string list
| UndefinedVars of string list

let typecheck_kernel (k:kernel) =
  let handle ctr errs l =
    if not (StringSet.is_empty l)
    then (StringSet.elements l |> ctr)::errs
    else errs
  in
  let dup_vars (l:string list) ctr errs =
    let rec iter (s:StringSet.t) (l:string list) =
      match l with
      | [] -> StringSet.empty
      | x::l ->
        let rest = iter (StringSet.remove x s) l in
        if StringSet.mem x s
        then rest
        else StringSet.add x rest
    in
    handle ctr errs (iter (StringSet.of_list l) l)
  in
  let undef_vars (vars:StringSet.t) (p:proto) errs : type_error list =
    StringSet.diff (Freenames.free_names_proto p StringSet.empty) vars
      |> handle (fun l -> UndefinedVars l) errs
  in
  let undef_locs (locs:StringSet.t) (p:proto) errs =
    StringSet.diff (Freenames.free_locs_proto p locs) locs
      |> handle (fun l -> UndefinedLocs l) errs
  in
  let errs = [] in
  let all_vars = List.append k.kernel_local_variables k.kernel_global_variables in
  let errs = dup_vars k.kernel_locations (fun l -> DuplicateLocs l) errs in
  let errs = dup_vars all_vars (fun l -> DuplicateVars l) errs in
  let errs = undef_vars (StringSet.of_list all_vars) k.kernel_code errs in
  let errs = undef_locs (StringSet.of_list k.kernel_locations) k.kernel_code errs in
  errs
