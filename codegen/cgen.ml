open Stage0
open Protocols
open Bc

open Exp
open Proto
open Serialize

module VarSet = Variable.Set
module VarMap = Variable.Map
module StringSet = Common.StringSet

(* ----------------- constants -------------------- *)
let thread_globals : VarSet.t =
  List.map Variable.from_name
    ["blockDim.x"; "blockIdx.x"; "gridDim.x"; "gridIdx.x"; "threadDim.x";
     "blockDim.y"; "blockIdx.y"; "gridDim.y"; "gridIdx.y"; "threadDim.y";
     "blockDim.z"; "blockIdx.z"; "gridDim.z"; "gridIdx.z"; "threadDim.z";
     "warpSize"]
  |> VarSet.of_list

let thread_locals : VarSet.t =
  List.map Variable.from_name
    ["threadIdx.x"; "threadIdx.y"; "threadIdx.z"]
  |> VarSet.of_list

let known_types : StringSet.t =
  ["__builtin_va_list"; "__float128"; "__int128_t"; "__uint128_t"; "bool";
   "char"; "char16_t"; "char32_t"; "cudaError_t"; "cudaStream"; "cudaStream_t";
   "cudaTextureObject_t"; "curandDiscreteDistribution_st";
   "curandDiscreteDistribution_t"; "curandState"; "curandStateMRG32k3a";
   "curandStateMRG32k3a_t"; "curandStateMtgp32"; "curandStateMtgp32_t";
   "curandStatePhilox4_32_10"; "curandStatePhilox4_32_10_t";
   "curandStateScrambledSobol32"; "curandStateScrambledSobol32_t";
   "curandStateScrambledSobol64"; "curandStateScrambledSobol64_t";
   "curandStateSobol32"; "curandStateSobol32_t"; "curandStateSobol64";
   "curandStateSobol64_t"; "curandStateXORWOW"; "curandStateXORWOW_t";
   "curandState_t"; "dim4"; "double"; "enumcudaError"; "float"; "int";
   "int16_t"; "int32_t"; "int64_t"; "int8_t"; "long"; "ptrdiff_t"; "short";
   "size_t"; "uint"; "uint16_t"; "uint32_t"; "uint8_t"; "ushort"; "wchar_t";
   "void"]
  |> StringSet.of_list

let vector_types : StringSet.t =
  ["char"; "double"; "float"; "int"; "long"; "longlong";
   "short"; "uchar"; "uint"; "ulong"; "ulonglong"; "ushort";]
  |> StringSet.of_list

let cuda_protos : string list =
  ["extern __device__ int __dummy_int();"]

let racuda_protos : string list =
  ["extern int __dummy_int();";]

(* ----------------- serialization -------------------- *)
let var_name (v : Variable.t) : string = v.name

(* Converts an array index to a string *)
let idx_to_s (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (Common.join "][" (List.map f l)) ^ "]"

(* Converts source expression to a RaCUDA-friendly form *)
let rec n_par (n : nexp) : string =
  match n with
  | Proj _
  | Num _
  | Var _
  | NCall _
    -> n_to_s n
  | NIf _
  | Bin _
    -> "(" ^ n_to_s n ^ ")"
and n_to_s : nexp -> string = function
  | Proj (t, x) ->
    "proj(" ^ task_to_string t ^ ", "  ^ var_name x ^ ")"
  | Num n -> string_of_int n
  | Var x -> var_name x
  | Bin (b, a1, a2) -> n_par a1 ^ " " ^ nbin_to_string b ^ " " ^ n_par a2
  | NCall (x, arg) ->
    x ^ "(" ^ n_to_s arg ^ ")"
  | NIf (b, n1, n2) ->
    b_par b ^ " ? " ^ n_par n1 ^ " : " ^ n_par n2
and b_to_s : bexp -> string = function
  | Bool b -> if b then "(0 == 0)" else "(0 != 0)"
  | NRel (b, n1, n2) ->
    n_to_s n1 ^ " " ^ PPrint.nrel_to_string b ^ " " ^ n_to_s n2
  | BRel (b, b1, b2) ->
    b_par b1 ^ " " ^ PPrint.brel_to_string b ^ " " ^ b_par b2
  | BNot b -> "!" ^ b_par b
  | Pred (x, v) -> x ^ "(" ^ n_to_s v ^ ")"
and b_par (b : bexp) : string =
  match b with
  | Pred _
  | Bool _
  | NRel _ -> b_to_s b
  | BNot _
  | BRel _ -> "("  ^ b_to_s b ^ ")"

(* Gives the dummy variable string for any variable *)
let var_to_dummy (v : Variable.t) : string =
  "__dummy" ^ var_name v

(* Gives the dummy instruction for any array read/write *)
let acc_expr_to_dummy (x, a) (racuda : bool) : PPrint.t list =
  let var = if racuda then var_name x ^ idx_to_s n_to_s a.access_index 
    else var_name x ^ idx_to_s PPrint.n_to_s a.access_index in
  match a.access_mode with
  | R -> [Line (var_to_dummy x ^ " = " ^ var ^ ";")]
  | W -> [Line (var ^ " = " ^ var_to_dummy x ^ "_w();")]

(* Converts a loop increment to a string *)
let inc_to_s (r : range) (n_to_s : nexp -> string) : string =
  let pred_to_s (pred : string) : string = 
    if String.starts_with ~prefix:"pow" pred
    then String.sub pred 3 (String.length pred - 3)
    else begin Printf.eprintf "WARNING: range step %s unsupported in range %s\n"
        pred (PPrint.r_to_s r);
      pred
    end
  in
  var_name r.range_var ^
  match r.range_step, r.range_dir with
  | Default step, Increase -> " += " ^ n_to_s step
  | Default step, Decrease -> " -= " ^ n_to_s step
  | StepName pred_name, Increase -> " *= " ^ pred_to_s pred_name
  | StepName pred_name, Decrease -> " /= " ^ pred_to_s pred_name

(* Converts source instruction to a valid CUDA operation *)
let rec inst_to_s (racuda : bool) : inst -> PPrint.t list = function
  | Acc e -> acc_expr_to_dummy e racuda
  | Sync -> [Line "__syncthreads();"]
  | Cond (b, p) ->
    [
      Line ("if (" ^ (if racuda then b_to_s else PPrint.b_to_s) b ^ ") {");
      Block (List.map (inst_to_s racuda) p |> List.flatten);
      Line "}"
    ]
  | Loop (r, p) ->
    let x = var_name r.range_var in
    let n_to_s = if racuda then n_to_s else PPrint.n_to_s in
    let lb, ub, op = match r.range_dir with
      | Increase -> r.range_lower_bound, r.range_upper_bound, " < "
      | Decrease -> n_plus (Num (-1)) r.range_upper_bound |> Constfold.n_opt,
                    n_plus (Num (-1)) r.range_lower_bound |> Constfold.n_opt,
                    " > "
    in
    [ 
      Line ("for (" ^ "int " ^ x ^ " = " ^ n_to_s lb ^ "; " ^ x
            ^ op ^ n_to_s ub ^ "; " ^ inc_to_s r n_to_s ^ ") {");
      Block (List.map (inst_to_s racuda) p |> List.flatten);
      Line "}"
    ]

(* Converts source instruction to the list of variables used in it *)
let rec inst_to_vars : inst -> Variable.t list = function
  | Acc (_, e) -> List.map n_to_vars e.access_index |> List.flatten
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
and r_to_vars (r : range) : Variable.t list =
  let step_variables = match r.range_step with
    | Default e -> n_to_vars e
    | StepName _ -> []
  in
  [[r.range_var]; n_to_vars r.range_lower_bound;
   n_to_vars r.range_upper_bound; step_variables]
  |> List.flatten

(* Gets the set of variables used in a list of instructions *)
let variables_used (l : inst list) : VarSet.t =
  List.map inst_to_vars l |> List.flatten |> VarSet.of_list

(* Flattens a multi-dimensional array into a 1D product of its dimensions *)
let flatten_multi_dim (arr : array_t) : array_t =
  match arr.array_size with
  | [] -> arr
  | size -> {arr with array_size = [List.fold_left ( * ) 1 size]}

(* Converts a shared access to a protocol instruction *)
let rec shared_access_to_inst : Shared_access.t -> inst = function
  | Loop (r, acc) -> Loop (r, [shared_access_to_inst acc])
  | Cond (b, acc) -> Cond (b, [shared_access_to_inst acc])
  (* Assume the access is a read *)
  | Index a -> Acc (a.shared_array, {access_index=[a.index]; access_mode=Exp.R})

(* Makes a kernel RaCUDA-friendly via protocol slicing *)
let mk_racuda_friendly (k : prog kernel) =
  let arrays = VarMap.mapi (fun _ -> flatten_multi_dim) k.kernel_arrays in
  let code = Shared_access.from_kernel (Vec3.make ~x:1024 ~y:1 ~z:1) k
             |> Seq.map shared_access_to_inst
             |> List.of_seq in
  {k with kernel_arrays = arrays; kernel_code = code}

(* Gets the type of an array, defaulting to int if it is unknown *)
let arr_type (arr : array_t) (strip_const : bool) : string =
  if arr.array_type = [] then "int"
  else if strip_const then List.filter (fun x -> x <> "const") arr.array_type
                           |> Common.join " "
  else Common.join " " arr.array_type  

(* Removes template parameters from a CUDA type *)
let remove_template (s : string) : string =
  match String.index_opt s '<' with
  | None -> s
  | Some t_index -> String.sub s 0 t_index

(* Replaces the type of each array with a compatible alternative *)
let mk_types_compatible
    (arrays : array_t VarMap.t)
    (racuda : bool)
  : array_t VarMap.t =
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
  let mk_array_compatible (arr : array_t) : array_t =
    { arr with
      array_type = convert_type (racuda && arr.array_hierarchy = SharedMemory)
          arr.array_type
    }
  in
  VarMap.mapi (fun _ -> fun v -> mk_array_compatible v) arrays

(* Checks if a string is a known CUDA type *)
let is_known_type (s : string) : bool =
  if StringSet.mem s known_types then true
  else match String.length s with
    | 0 -> true
    | len -> StringSet.mem (String.sub s 0 (len - 1)) vector_types
             && match s.[len - 1] with
             | '1' | '2' | '3' | '4' -> true
             | _ -> false

let declare_unknown_types (vm : array_t VarMap.t) : string list =
  (* Converts an array type to a declaration, or None if it is a known type *)
  let rec arr_type_to_decl : string list -> string option = function
    | [] -> None
    | [last_type] -> if is_known_type last_type then None
      else Some ("class " ^ last_type ^ " {};")
    | _ :: types -> arr_type_to_decl types
  in
  VarMap.bindings vm
  |> List.filter_map (fun (_, v) -> arr_type_to_decl v.array_type)
  (* Remove duplicates *)
  |> StringSet.of_list
  |> StringSet.elements

(* Include prototypes for bitwise operators in RaCUDA-friendly kernels *)
let base_protos (racuda : bool) : string list =
  if racuda then racuda_protos else cuda_protos

(* Create external function prototypes for writing to each array *)
let arr_to_proto (vm : array_t VarMap.t) (racuda : bool) : string list =
  let modifiers = if racuda then "extern " else "extern __device__ " in
  VarMap.bindings vm 
  |> List.map (fun (k, v) -> modifiers ^ arr_type v true ^ " "
                             ^ var_to_dummy k ^ "_w();")

(* Helper functions for making kernel parameters *)
let global_arr_to_l (vm : array_t VarMap.t) : string list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> arr_type v false ^ " *" ^ var_name k)

let global_var_to_l (vs : VarSet.t) : string list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> "int " ^ var_name v)

(* Helper functions for making kernel variable declarations *)
let arr_to_shared (vm : array_t VarMap.t) : PPrint.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      PPrint.Line ((if v.array_size = [] then "extern " else "") 
                   ^ "__shared__ " ^ arr_type v false ^ " " ^ var_name k
                   ^ idx_to_s string_of_int v.array_size ^ ";"))

let local_var_to_l (vs : VarSet.t) : PPrint.t list =
  (* A local variable must not be a tid/dummy variable *)
  VarSet.diff vs thread_locals
  |> VarSet.filter (fun v -> not
                       (String.starts_with ~prefix:"__dummy" (var_name v)))
  |> VarSet.elements
  (* Use a single function to initialize all local variables *)
  |> List.map (fun v -> PPrint.Line ("int " ^ var_name v ^ " = __dummy_int();"))

let arr_to_dummy (vm : array_t VarMap.t) : PPrint.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      PPrint.Line (arr_type v true ^ " " ^ var_to_dummy k ^ ";"))

(* Serialization of the kernel body *)
let body_to_s (f : prog -> PPrint.t list) (k : prog kernel) : PPrint.t =
  let shared_arr = arr_to_shared (VarMap.filter (fun _ -> fun v ->
      v.array_hierarchy = SharedMemory) k.kernel_arrays) in
  let used_var = variables_used k.kernel_code in
  (* Remove unused local variables *)
  let local_var = VarSet.inter used_var k.kernel_local_variables
                  |> local_var_to_l in
  let dummy_var = arr_to_dummy k.kernel_arrays in
  PPrint.Block (shared_arr @ local_var @ dummy_var @ (f k.kernel_code))

(* Serialization of the kernel *)
let kernel_to_s
    (f : prog -> PPrint.t list)
    (k : prog kernel)
    (racuda : bool)
  : PPrint.t list =
  let open PPrint in
  let k = {k with kernel_arrays = mk_types_compatible k.kernel_arrays racuda} in
  let type_decls = if racuda then []
    else declare_unknown_types k.kernel_arrays in
  let funct_protos = base_protos racuda @ arr_to_proto k.kernel_arrays racuda in
  let k_name = if k.kernel_name = "main" then "kernel" else k.kernel_name in
  let global_arr = global_arr_to_l (VarMap.filter (fun _ -> fun v ->
      v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let global_var = global_var_to_l k.kernel_global_variables in
  let params = global_arr @ global_var in
  [
    Line (Common.join "\n" (type_decls @ funct_protos));
    Line ("__global__ void " ^ k_name ^ "(" ^ Common.join ", " params ^ ")");
    Line "{";
    (body_to_s f k);
    Line "}"
  ]

let prog_to_s (racuda : bool) (p : prog) : PPrint.t list =
  p_opt p |> List.map (inst_to_s racuda) |> List.flatten

let print_k (k : prog kernel) (racuda : bool) : unit =
  let k = if racuda then mk_racuda_friendly k else k in
  PPrint.print_doc (kernel_to_s (prog_to_s racuda) k racuda)

(* Kernel to TOML conversion *)
let arrays_to_l (vm : array_t VarMap.t) : (string * Otoml.t) list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> (var_name k, Otoml.TomlString (arr_type v false)))

let scalars_to_l (vs : VarSet.t) : (string * Otoml.t) list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> (var_name v, Otoml.TomlString "int"))

let kernel_to_toml (k : prog kernel) (racuda : bool) : Otoml.t =
  let k = {k with kernel_arrays = mk_types_compatible k.kernel_arrays racuda} in
  let type_decls = if racuda then []
    else declare_unknown_types k.kernel_arrays in
  let funct_protos = base_protos racuda @ arr_to_proto k.kernel_arrays racuda in
  let header = (type_decls @ funct_protos |> Common.join "\n") ^ "\n" in
  let body = PPrint.doc_to_string [body_to_s (prog_to_s racuda) k] in
  let global_arr = VarMap.filter (fun _ -> fun v ->
      v.array_hierarchy = GlobalMemory) k.kernel_arrays in
  let open Otoml in
  TomlTable
    [
      ("pass", TomlBoolean true);
      ("includes", TomlArray []);
      ("header", TomlString header);
      ("body", TomlString body);
      ("scalars", TomlTable (scalars_to_l k.kernel_global_variables));
      ("arrays", TomlTable (arrays_to_l global_arr));
    ]

let print_toml (table : Otoml.t) : unit =
  print_string (Otoml.Printer.to_string table)

let print_t (k : prog kernel) (racuda : bool) : unit =
  print_toml (kernel_to_toml k racuda)
