open Protocols
open Bank_conflicts
open Exp
open Proto

module VarSet = Variable.Set
module VarMap = Variable.Map

(* Make blockDim parameters RaCUDA-friendly *)
let prepare_params (g : Generator.t) (gv : Gv_parser.t)
  : Gv_parser.t * Params.t =
  let f x = if not g.mod_gv_args then x else if x > 1 then max x 32 else 1 in
  let x, y, z = gv.block_dim.x, gv.block_dim.y, gv.block_dim.z in
  let block_dim = Dim3.{x = f x; y = f y; z = f z} in
  let grid_dim = gv.grid_dim in
  { gv with block_dim }, Params.make ~block_dim ~grid_dim ()

(* Use constant folding to simplify the code *)
let constant_folding (k : prog kernel) : prog kernel =
  let code = p_opt k.kernel_code in
  {k with kernel_code = code}

(* Convert source instruction to the list of variables used in it *)
let rec inst_to_vars : inst -> Variable.t list = function
  | Acc (_, e) -> List.map n_to_vars e.index |> List.flatten
  | Sync -> []
  | Cond (b, p) -> b_to_vars b :: List.map inst_to_vars p |> List.flatten
  | Loop (r, p) -> r_to_vars r :: List.map inst_to_vars p |> List.flatten
and n_to_vars : nexp -> Variable.t list = function
  | Var x -> [x]
  | Num _ -> []
  | Bin (_, e1, e2) -> n_to_vars e1 @ n_to_vars e2
  | Proj (_, x) -> [x]
  | NCall (_, e) -> n_to_vars e
  | NIf (b, e1, e2) -> b_to_vars b @ n_to_vars e1 @ n_to_vars e2
and b_to_vars : bexp -> Variable.t list = function
  | Bool _ -> []
  | NRel (_, e1, e2) -> n_to_vars e1 @ n_to_vars e2
  | BRel (_, b1, b2) -> b_to_vars b1 @ b_to_vars b2
  | BNot b -> b_to_vars b
  | Pred (_, e) -> n_to_vars e
and r_to_vars (r : Range.t) : Variable.t list =
  let step_variables = match r.step with
    | Plus e -> n_to_vars e
    | Mult e -> n_to_vars e
  in
  [[r.var]; n_to_vars r.lower_bound; n_to_vars r.upper_bound; step_variables]
  |> List.flatten

(* Get the set of variables used in the code *)
let variables_used (p : prog) : VarSet.t =
  List.map inst_to_vars p |> List.flatten |> VarSet.of_list

(* Remove unused local variables from the kernel *)
let remove_unused_variables (k : prog kernel) : prog kernel =
  let used_variables = variables_used k.kernel_code in
  let local_variables = VarSet.inter used_variables k.kernel_local_variables in
  {k with kernel_local_variables = local_variables}

(* Remove template parameters from a CUDA type *)
let remove_template (s : string) : string =
  match String.index_opt s '<' with
  | None -> s
  | Some t_index -> String.sub s 0 t_index

(* Convert a dependent/templated type to a generic CUDA type *)
let rec convert_type : string list -> string list = function
  | [] -> []
  | [last_type] -> [remove_template last_type]
  | modifier :: types ->
    if modifier = "typename" then convert_type types
    else modifier :: convert_type types

(* Replace the type of each array with a compatible alternative *)
let mk_types_compatible (k : prog kernel) : prog kernel =
  let mk_array_compatible (arr : Memory.t) : Memory.t =
    {arr with data_type = convert_type arr.data_type}
  in
  let arrays = VarMap.map mk_array_compatible k.kernel_arrays in
  {k with kernel_arrays = arrays}

(* Prepare the kernel for serialization *)
let prepare_kernel (g : Generator.t) (params : Params.t) (k : prog kernel)
  : prog kernel =
  k
  |> (if g.const_fold then constant_folding else Fun.id)
  |> remove_unused_variables
  |> mk_types_compatible
  |> (if g.distinct_vars then kernel_vars_distinct else Fun.id)
  |> (if g.simplify_kernel then Shared_access.Silent.simplify_kernel params
      else Fun.id)
