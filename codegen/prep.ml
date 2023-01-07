open Protocols
open Bank_conflicts
open Exp
open Proto

module VarSet = Variable.Set
module VarMap = Variable.Map

(* Rename the kernel to prevent parsing errors *)
let rename_kernel (k : prog kernel) : prog kernel =
  let name = if k.kernel_name = "main" then "kernel" else k.kernel_name in
  {k with kernel_name = name}

(* Use constant folding to simplify the code *)
let constant_folding (k : prog kernel) : prog kernel =
  {k with kernel_code = p_opt k.kernel_code}

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

(* Remove unused local variables from the kernel  *)
let remove_unused_variables (k : prog kernel) : prog kernel =
  let used_variables = variables_used k.kernel_code in
  let local_variables = VarSet.inter used_variables k.kernel_local_variables in
  {k with kernel_local_variables = local_variables}

(* Remove template parameters from a CUDA type *)
let remove_template (s : string) : string =
  match String.index_opt s '<' with
  | None -> s
  | Some t_index -> String.sub s 0 t_index

(* Replace the type of each array with a compatible alternative *)
let mk_types_compatible (racuda : bool) (k : prog kernel) : prog kernel =
  let rec convert_type (racuda_shared : bool) : string list -> string list =
    function
    | [] -> []
    (* Unknown/incompatible types are converted to int in RaCUDA output *)
    | [last_type] -> if racuda then
        match last_type with
        | "char" | "double" | "float" | "int" | "void" -> [last_type]
        | "long" | "short" -> if racuda_shared then ["int"] else [last_type]
        | _ -> ["int"]
      else [remove_template last_type]
    (* Remove unsigned modifier to make shared arrays RaCUDA-friendly *)
    | modifier :: types -> if racuda_shared && modifier = "unsigned"
      then convert_type racuda_shared types
      else modifier :: convert_type racuda_shared types
  in
  let mk_array_compatible (arr : Memory.t) : Memory.t =
    { arr with
      data_type = arr.data_type
                  |> convert_type (racuda && arr.hierarchy = SharedMemory)
    }
  in
  let arrays = k.kernel_arrays
               |> VarMap.mapi (fun _ -> fun v -> mk_array_compatible v)
  in
  {k with kernel_arrays = arrays}

(* Set the number of threads per block *)
let set_block_dim (thread_count : Vec3.t) (k : prog kernel) : prog kernel =
  let subst (x : string) (n : int) (p : prog) : prog =
    Proto.PSubstPair.p_subst (Variable.from_name x, Num n) p
  in
  let code = k.kernel_code
             |> subst "blockDim.x" thread_count.x
             |> subst "blockDim.y" thread_count.y
             |> subst "blockDim.z" thread_count.z
  in
  {k with kernel_code = code}

(* Convert a shared access to a protocol instruction *)
let rec shared_access_to_inst : Shared_access.t -> inst = function
  | Loop (r, acc) -> Loop (r, [shared_access_to_inst acc])
  | Cond (b, acc) -> Cond (b, [shared_access_to_inst acc])
  (* Assume the access is a read *)
  | Index a -> Acc (a.shared_array, {index=[a.index]; mode=Rd})

(* Slice a protocol into independently-analyzable shared accesses *)
let slice_protocol (thread_count : Vec3.t) (k : prog kernel) : prog kernel =
  let code = Shared_access.from_kernel thread_count k
             |> Seq.map shared_access_to_inst
             |> List.of_seq
  in
  {k with kernel_code = code}

(* Flatten multi-dimensional arrays into 1D products of their dimensions *)
let flatten_multi_dim (k : prog kernel) : prog kernel =
  let flatten_array (arr : Memory.t) : Memory.t =
    match arr.size with
    | [] -> arr
    | size -> {arr with size = [List.fold_left ( * ) 1 size]}
  in
  let arrays = VarMap.mapi (fun _ -> flatten_array) k.kernel_arrays in
  {k with kernel_arrays = arrays}

(* Prepare the kernel for serialization *)
let prepare_kernel
    (racuda : bool)
    (thread_count : Vec3.t)
    (k : prog kernel)
  : prog kernel =
  let k = k
          |> rename_kernel
          |> constant_folding
          |> remove_unused_variables
          |> mk_types_compatible racuda
  in
  if racuda then k
                 |> set_block_dim thread_count
                 |> slice_protocol thread_count
                 |> flatten_multi_dim
  else k
