open Stage0
open Protocols
open Exp
open Proto

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
   "short"; "uchar"; "uint"; "ulong"; "ulonglong"; "ushort"]
  |> StringSet.of_list

let cuda_protos : string list = ["extern __device__ int __dummy_int();"]

let racuda_protos : string list =
  ["extern __attribute__((device)) int __dummy_int();"]

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
    n_to_s n1 ^ " " ^ Exp.nrel_to_string b ^ " " ^ n_to_s n2
  | BRel (b, b1, b2) ->
    b_par b1 ^ " " ^ Exp.brel_to_string b ^ " " ^ b_par b2
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
let var_to_dummy (v : Variable.t) : string = "__dummy" ^ var_name v

(* Gives the dummy instruction for any array read/write *)
let acc_expr_to_dummy (x, a : Variable.t * Access.t) (racuda : bool)
  : Indent.t list =
  let n_to_s = if racuda then n_to_s else Exp.n_to_string in
  let var = var_name x ^ idx_to_s n_to_s a.Access.index in
  match a.Access.mode with
  | Rd -> [Line (var_to_dummy x ^ " = " ^ var ^ ";")]
  | Wr -> [Line (var ^ " = " ^ var_to_dummy x ^ "_w();")]

(* Converts a loop increment to a string *)
let inc_to_s (r : Range.t) (n_to_s : nexp -> string) : string =
  var_name r.var ^
  match r.step, r.dir with
  | Plus step, Increase -> " += " ^ n_to_s step
  | Plus step, Decrease -> " -= " ^ n_to_s step
  | Mult step, Increase -> " *= " ^ n_to_s step
  | Mult step, Decrease -> " /= " ^ n_to_s step

(* Converts source instruction to a valid CUDA operation *)
let rec inst_to_s (racuda : bool) : inst -> Indent.t list = function
  | Acc e -> acc_expr_to_dummy e racuda
  | Sync -> [Line "__syncthreads();"]
  | Cond (b, p) ->
    [
      Line ("if (" ^ (if racuda then b_to_s else Exp.b_to_string) b ^ ") {");
      Block (List.map (inst_to_s racuda) p |> List.flatten);
      Line "}"
    ]
  | Loop (r, p) ->
    let x = var_name r.var in
    let n_to_s = if racuda then n_to_s else Exp.n_to_string in
    let lb, ub, op = match r.dir with
      | Increase -> r.lower_bound, r.upper_bound, " < "
      | Decrease -> n_dec r.upper_bound, n_dec r.lower_bound, " > "
    in
    [ 
      Line ("for (" ^ "int " ^ x ^ " = " ^ n_to_s lb ^ "; " ^ x
            ^ op ^ n_to_s ub ^ "; " ^ inc_to_s r n_to_s ^ ") {");
      Block (List.map (inst_to_s racuda) p |> List.flatten);
      Line "}"
    ]

(* Checks if a string is a known CUDA type *)
let is_known_type (s : string) : bool =
  if StringSet.mem s known_types then true
  else match String.length s with
    | 0 -> true
    (* Checks if a string is a vector type *)
    | len -> StringSet.mem (String.sub s 0 (len - 1)) vector_types
             && match s.[len - 1] with
             | '1' | '2' | '3' | '4' -> true
             | _ -> false

(* Creates empty class declarations for unknown types *)
let decl_unknown_types (vm : Memory.t VarMap.t) : string list =
  (* Converts an array type to a declaration, or None if it is a known type *)
  let rec arr_type_to_decl : string list -> string option = function
    | [] -> None
    | [last_type] -> if is_known_type last_type then None
      else Some ("class " ^ last_type ^ " {};")
    | _ :: types -> arr_type_to_decl types
  in
  VarMap.bindings vm
  |> List.filter_map (fun (_, v) -> arr_type_to_decl v.Memory.data_type)
  (* Remove duplicates *)
  |> StringSet.of_list
  |> StringSet.elements

(* Expand the __device__ modifier to make kernels RaCUDA-friendly *)
let base_protos (racuda : bool) : string list =
  if racuda then racuda_protos else cuda_protos

(* Gets the type of an array, defaulting to int if it is unknown *)
let arr_type ?(strip_const=false) (arr : Memory.t) : string =
  if arr.data_type = [] then "int"
  else if strip_const then
    List.filter (fun x -> x <> "const") arr.data_type |> Common.join " "
  else Common.join " " arr.data_type

(* Create external function prototypes for writing to each array *)
let arr_to_proto (vm : Memory.t VarMap.t) (racuda : bool) : string list =
  let modifiers = 
    "extern " ^ if racuda then "__attribute__((device)) " else "__device__ "
  in
  VarMap.bindings vm 
  |> List.map (fun (k, v) -> modifiers ^ arr_type v ~strip_const:true
                             ^ " " ^ var_to_dummy k ^ "_w();")

(* Helper functions for making kernel parameters *)
let global_arr_to_l (vm : Memory.t VarMap.t) : string list =
  VarMap.bindings vm |> List.map (fun (k, v) -> arr_type v ^ " *" ^ var_name k)

let global_var_to_l (vs : VarSet.t) : string list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> "int " ^ var_name v)

(* Helper functions for making kernel variable declarations *)
let arr_to_shared (vm : Memory.t VarMap.t) : Indent.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      Indent.Line ((if v.Memory.size = [] then "extern " else "") 
                   ^ "__shared__ " ^ arr_type v ^ " " ^ var_name k
                   ^ idx_to_s string_of_int v.Memory.size ^ ";"))

let local_var_to_l (vs : VarSet.t) : Indent.t list =
  (* A local variable must not be a tid/dummy variable *)
  VarSet.diff vs thread_locals
  |> VarSet.filter (fun v ->
      not (String.starts_with ~prefix:"__dummy" (var_name v)))
  |> VarSet.elements
  (* Use a single function to initialize all local variables *)
  |> List.map (fun v -> Indent.Line ("int " ^ var_name v ^ " = __dummy_int();"))

let arr_to_dummy (vm : Memory.t VarMap.t) : Indent.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      Indent.Line (arr_type v ~strip_const:true ^ " " ^ var_to_dummy k ^ ";"))

(* Serialization of the kernel header *)
let header_to_s (racuda : bool) (k : prog kernel) : Indent.t =
  let type_decls = if racuda then [] else decl_unknown_types k.kernel_arrays in
  let funct_protos = base_protos racuda @ arr_to_proto k.kernel_arrays racuda in
  Indent.Line (type_decls @ funct_protos |> Common.join "\n")

(* Serialization of the kernel body *)
let body_to_s (f : prog -> Indent.t list) (k : prog kernel) : Indent.t =
  let shared_arr = k.kernel_arrays
                   |> VarMap.filter (fun _ -> Memory.is_shared)
                   |> arr_to_shared
  in
  let local_var = local_var_to_l k.kernel_local_variables in
  let dummy_var = arr_to_dummy k.kernel_arrays in
  Indent.Block (shared_arr @ local_var @ dummy_var @ (f k.kernel_code))

(* Serialization of the kernel *)
let kernel_to_s (f : prog -> Indent.t list) (racuda : bool) (k : prog kernel)
  : Indent.t list =
  let global_arr = k.kernel_arrays
                   |> VarMap.filter (fun _ -> Memory.is_global)
                   |> global_arr_to_l
  in
  let global_var = global_var_to_l k.kernel_global_variables in
  let params = global_arr @ global_var |> Common.join ", " in
  [
    header_to_s racuda k;
    Line ("__global__ void " ^ k.kernel_name ^ "(" ^ params ^ ")");
    Line "{";
    body_to_s f k;
    Line "}"
  ]

let prog_to_s (racuda : bool) (p : prog) : Indent.t list =
  List.map (inst_to_s racuda) p |> List.flatten

let gen_cuda (racuda : bool) (k : prog kernel) : string =
  kernel_to_s (prog_to_s racuda) racuda k |> Indent.to_string
