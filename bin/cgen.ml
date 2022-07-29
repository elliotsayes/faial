open Exp
open Proto
open Serialize

(* ----------------- constants -------------------- *)
let thread_globals : VarSet.t =
  (List.map var_make 
     ["blockDim.x"; "blockIdx.x"; "gridDim.x"; "gridIdx.x"; "threadDim.x";
      "blockDim.y"; "blockIdx.y"; "gridDim.y"; "gridIdx.y"; "threadDim.y";
      "blockDim.z"; "blockIdx.z"; "gridDim.z"; "gridIdx.z"; "threadDim.z"])
  |> VarSet.of_list

let thread_locals : VarSet.t =
  (List.map var_make
     ["threadIdx.x"; "threadIdx.y"; "threadIdx.z"])
  |> VarSet.of_list

let cpp_types : Common.StringSet.t =
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
   "size_t"; "uint"; "uint16_t"; "uint32_t"; "uint8_t"; "ushort"; "wchar_t";]
  |> Common.StringSet.of_list

let vector_types : Common.StringSet.t =
  ["char"; "double"; "float"; "int"; "long"; "longlong";
   "short"; "uchar"; "uint"; "ulong"; "ulonglong"; "ushort";]
  |> Common.StringSet.of_list

let cuda_protos : string list =
  ["extern __device__ int __dummy_int();"]

let racuda_protos : string list =
  ["extern int __dummy_int();";
   "extern int __bor(int, int);";
   "extern int __bxor(int, int);";
   "extern int __band(int, int);";
   "extern int __lshift(int, int);";
   "extern int __rshift(int, int);"]

(* ----------------- serialization -------------------- *)
let join (sep : string) (elems : string list) : string =
  List.rev elems |> Common.join sep

(* Gives the dummy variable string for any variable *)
let var_to_dummy (v : variable) : string =
  "__dummy" ^ var_name v

(* Maps a list of values to an index-separated string containing them *)
let idx_to_s (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (join "][" (List.map f l)) ^ "]"

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
  | Bin (b, a1, a2) ->
    (match b with
     | BitOr -> "__bor(" ^ n_par a1 ^ ", " ^ n_par a2 ^ ")"
     | BitXOr -> "__bxor(" ^ n_par a1 ^ ", " ^ n_par a2 ^ ")"
     | BitAnd -> "__band(" ^ n_par a1 ^ ", " ^ n_par a2 ^ ")"
     | LeftShift -> "__lshift(" ^ n_par a1 ^ ", " ^ n_par a2 ^ ")"
     | RightShift -> "__rshift(" ^ n_par a1 ^ ", " ^ n_par a2 ^ ")"
     | _ -> n_par a1 ^ " " ^ nbin_to_string b ^ " " ^ n_par a2)
  | NCall (x, arg) ->
    x ^ "(" ^ n_to_s arg ^ ")"
  | NIf (b, n1, n2) ->
    b_par b ^ " ? " ^ n_par n1 ^ " : " ^ n_par n2
and b_to_s : bexp -> string = function
  | Bool b -> if b then "true" else "false"
  | NRel (b, n1, n2) ->
    n_to_s n1 ^ " " ^ PPrint.nrel_to_string b ^ " " ^ n_to_s n2
  | BRel (b, b1, b2) ->
    b_par b1 ^ " " ^ brel_to_string b ^ " " ^ b_par b2
  | BNot b -> "!" ^ b_par b
  | Pred (x, v) -> x ^ "(" ^ n_to_s v ^ ")"
and b_par (b : bexp) : string =
  match b with
  | Pred _
  | Bool _
  | NRel _ -> b_to_s b
  | BNot _
  | BRel _ -> "("  ^ b_to_s b ^ ")"
and s_to_s s =
  match s with
  | Default x -> "+ " ^ n_to_s x
  | StepName "pow2" -> "* 2"
  | StepName x -> x

(* Gives the dummy instruction for any array read/write *)
let acc_expr_to_dummy (x, a) (racuda : bool) : PPrint.t list =
  let var = if racuda then var_name x ^ idx_to_s n_to_s a.access_index 
    else var_name x ^ idx_to_s PPrint.n_to_s a.access_index in
  [Line (match a.access_mode with
       | R -> var_to_dummy x ^ " = " ^ var ^ ";"
       | W -> var ^ " = " ^ var_to_dummy x ^ "_w();")]

(* Converts source instruction to a valid CUDA operation *)
let rec inst_to_s (racuda : bool) : inst -> PPrint.t list =
  function
  | Sync -> [Line "__syncthreads();"]
  | Acc e -> (acc_expr_to_dummy e racuda)
  | Cond (b, p1) -> [
      Line ("if (" ^ (if racuda then b_to_s else PPrint.b_to_s) b ^ ") {");
      Block (List.map (inst_to_s racuda) p1 |> List.flatten);
      Line "}"
    ]
  | Loop (r, p) ->
    [ 
      Line ("for (" ^ "int " ^ var_name r.range_var ^ " = "
            ^ n_to_s r.range_lower_bound ^ "; " ^ var_name r.range_var ^ " < "
            ^ n_to_s r.range_upper_bound ^ "; " ^ var_name r.range_var ^ " = "
            ^ var_name r.range_var ^ " "
            ^ (if racuda then s_to_s else PPrint.s_to_s) r.range_step ^ ") {");
      Block (List.map (inst_to_s racuda) p |> List.flatten);
      Line "}" 
    ]

(* Get the type of an array, defaulting to int if it is unknown *)
let arr_type (arr : array_t) (strip_const : bool) (racuda : bool) : string =
  let mod_filter (modifier : string) : bool =
    match strip_const, racuda with
    | true, true -> modifier <> "const" && modifier <> "unsigned"
    | true, false -> modifier <> "const"
    | false, true -> modifier <> "unsigned"
    | false, false -> true
  in
  if arr.array_type = [] then "int"
  else List.filter mod_filter arr.array_type |> join " "

(* Include prototypes for bitwise operators in RaCUDA-friendly kernels *)
let base_protos (racuda : bool) : string list =
  if racuda then racuda_protos else cuda_protos

(* Create external function prototypes for writing to each array *)
let arr_to_proto (vm : array_t VarMap.t) (racuda : bool) : string list =
  let modifiers = if racuda then "extern " else "extern __device__ " in
  VarMap.bindings vm 
  |> List.map (fun (k, v) -> (modifiers ^ arr_type v true racuda
                              ^ " " ^ var_to_dummy k ^ "_w();"))

(* Checks if a string is a known C++/CUDA type *)
let is_known_type (s : string) : bool =
  if Common.StringSet.mem s cpp_types then true
  else match String.length s with
    | 0 -> true
    | len -> Common.StringSet.mem (String.sub s 0 (len - 1)) vector_types
             && match s.[len - 1] with
             | '1' | '2' | '3' | '4' -> true
             | _ -> false

let declare_unknown_types (vm : array_t VarMap.t) : string list =
  (* Converts an array type to a declaration, or None if it is a known type *)
  let rec arr_type_to_decl = function
    | [] -> None
    | [ last_type ] -> if is_known_type last_type then None
      else Some ("class " ^ last_type ^ " {};")
    | _ :: types -> arr_type_to_decl types
  in
  VarMap.bindings vm
  |> List.filter_map (fun (k, v) -> arr_type_to_decl v.array_type)
  (* Remove duplicates *)
  |> Common.StringSet.of_list
  |> Common.StringSet.elements

let global_arr_to_l (vm : array_t VarMap.t) (racuda : bool) : string list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> arr_type v false racuda ^ " *" ^ var_name k)

let global_var_to_l (vs : VarSet.t) : string list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> "int " ^ var_name v)

(* Helper functions for making kernel variable declarations *)
let arr_to_shared (vm : array_t VarMap.t) (racuda : bool) : PPrint.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      PPrint.Line ((match v.array_size with | [] -> "extern " | _ -> "") 
                   ^ "__shared__ " ^ arr_type v false racuda ^ " " ^ var_name k
                   ^ idx_to_s string_of_int v.array_size ^ ";"))

let arr_to_dummy (vm : array_t VarMap.t) (racuda : bool) : PPrint.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      PPrint.Line (arr_type v true racuda ^ " " ^ var_to_dummy k ^ ";"))

let local_var_to_l (vs : VarSet.t) : PPrint.t list =
  (* A local variable must not be a tid/dummy variable *)
  VarSet.filter (fun v -> not (VarSet.mem v thread_locals ||
                               String.starts_with "__dummy" (var_name v))) vs
  |> VarSet.elements
  (* Use a single function to initialize all local variables *)
  |> List.map (fun v -> PPrint.Line ("int " ^ var_name v ^ " = __dummy_int();"))

let body_to_s
    (f : 'a -> PPrint.t list)
    (k : 'a kernel)
    (racuda : bool) : PPrint.t
  =
  let shared_arr = arr_to_shared (VarMap.filter (fun k -> fun v ->
      v.array_hierarchy = SharedMemory) k.kernel_arrays) racuda in
  let local_var = local_var_to_l k.kernel_local_variables in
  let dummy_var = arr_to_dummy k.kernel_arrays racuda in
  PPrint.Block (shared_arr @ local_var @ dummy_var @ (f k.kernel_code))

(* Serialization of the kernel *)
let kernel_to_s
    (f : 'a -> PPrint.t list)
    (k : 'a kernel)
    (racuda : bool) : PPrint.t list
  =
  let open PPrint in
  let unknown_type_decls = declare_unknown_types k.kernel_arrays in
  let funct_protos = (base_protos racuda) @
                     (arr_to_proto k.kernel_arrays racuda) in
  let k_name = if k.kernel_name = "main" then "kernel" else k.kernel_name in
  let global_arr = global_arr_to_l (VarMap.filter (fun k -> fun v ->
      v.array_hierarchy = GlobalMemory) k.kernel_arrays) racuda in
  let global_var = global_var_to_l k.kernel_global_variables in
  [
    Line (join "\n" (unknown_type_decls @ funct_protos));
    Line "__global__";
    Line ("void " ^ k_name ^ "(" ^ join ", " (global_arr @ global_var) ^ ")");
    Line "{";
    (body_to_s f k racuda);
    Line "}"
  ]

let prog_to_s (racuda : bool) (p : prog) : PPrint.t list =
  List.map (inst_to_s racuda) p |> List.flatten

let print_k (k : prog kernel) (racuda : bool) : unit =
  PPrint.print_doc (kernel_to_s (prog_to_s racuda) k racuda)

(* Kernel to TOML conversion *)
open Toml.Min
open Toml.Types

let arrays_to_l (vm : array_t VarMap.t) (racuda : bool) : table list =
  VarMap.bindings vm
  |> List.map (fun (k, v) ->
      of_key_values [key (var_name k), TString (arr_type v false racuda)])

let scalars_to_l (vs : VarSet.t) : table list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> of_key_values [key (var_name v), TString "int"])

let kernel_to_toml (k : prog kernel) (racuda : bool) : table =
  let body = [body_to_s (prog_to_s racuda) k racuda] in
  let funct_protos = (base_protos racuda) @
                     (arr_to_proto k.kernel_arrays racuda) in
  let unknown_type_decls = declare_unknown_types k.kernel_arrays in
  let header = unknown_type_decls @ funct_protos in
  let global_arr = (VarMap.filter (fun k -> fun v ->
      v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let open PPrint in
  [
    key "body", TString (doc_to_string (Line "" :: body));
    key "header", TString ("\n" ^ (join "\n" header) ^ "\n");
    key "includes", TArray (NodeInt []);
    key "pass", TBool true;
    key "arrays", TArray (NodeTable (arrays_to_l global_arr racuda));
    key "scalars", TArray (NodeTable (scalars_to_l k.kernel_global_variables));
  ]
  |> of_key_values

let print_toml (table : table) : unit =
  print_string (Toml.Printer.string_of_table table)
