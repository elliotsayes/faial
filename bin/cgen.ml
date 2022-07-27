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

(* ----------------- serialization -------------------- *)

(* Gives the dummy variable string for any variable *)
let var_to_dummy (v : variable) : string =
  "__dummy" ^ var_name v

(* Maps a list of values to an index-separated string containing them *)
let idx_to_s (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (Common.join "][" (List.map f l |> List.rev)) ^ "]"

(* Gives the dummy instruction string for any array read/write *)
let acc_expr_to_dummy (x, a) : PPrint.t list =
  let var = var_name x ^ idx_to_s PPrint.n_to_s a.access_index in
  [Line (match a.access_mode with
       | R -> var_to_dummy x ^ " = " ^ var ^ ";"
       | W -> var ^ " = " ^ var_to_dummy x ^ "_w();")]

(* Converts source instruction to a valid CUDA operation *)
let rec inst_to_s : inst -> PPrint.t list =
  let open PPrint in
  function
  | Sync -> [Line "__syncthreads();"]
  | Acc e -> acc_expr_to_dummy e
  | Cond (b, p1) -> [
      Line ("if (" ^ b_to_s b ^ ") {");
      Block (List.map inst_to_s p1 |> List.flatten);
      Line "}"
    ]
  | Loop (r, p) ->
    [ 
      Line ("for (" ^ "int " ^ ident r.range_var ^ " = " ^
            n_to_s r.range_lower_bound ^ "; " ^ ident r.range_var ^ " < " ^
            n_to_s r.range_upper_bound ^ "; " ^ ident r.range_var ^ " = " ^
            ident r.range_var ^ " " ^ s_to_s r.range_step ^ ") {");
      Block (List.map inst_to_s p |> List.flatten);
      Line "}" 
    ]

let prog_to_s (p : prog) : PPrint.t list =
  List.map inst_to_s p |> List.flatten

(* Get the type of an array, defaulting to int if it is unknown *)
let arr_type (arr : array_t) (strip_const : bool) : string =
  if arr.array_type = [] then "int"
  else if strip_const then Common.join " "
      (List.filter (fun x -> x <> "const") arr.array_type)
  else Common.join " " arr.array_type

(* Checks if a string is a known C++/CUDA type *)
let is_known_type (s : string) : bool =
  if Common.StringSet.mem s cpp_types then true
  else match String.length s with
    | 0 -> true
    | len -> Common.StringSet.mem (String.sub s 0 (len - 1)) vector_types
             && match s.[len - 1] with
             | '1' | '2' | '3' | '4' -> true
             | _ -> false

(* Helper functions for making the kernel header/parameters *)
let declare_unknown_types (vm : array_t VarMap.t) =
  (* Converts an array type to a declaration, or None if it is a known type *)
  let rec arr_type_to_decl = function
    | [] -> None
    | [ last_type ] -> if is_known_type last_type then None
      else Some ("class " ^ last_type ^ " {};")
    | _ :: types -> arr_type_to_decl types
  in
  VarMap.bindings vm |>
  List.filter_map (fun (k, v) -> arr_type_to_decl v.array_type) |>
  (* Remove duplicates *)
  Common.StringSet.of_list |>
  Common.StringSet.elements |>
  function
  | [] -> PPrint.Nil
  | types -> PPrint.Line (Common.join "\n" types)

let global_arr_to_l (vm : array_t VarMap.t) : string list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> arr_type v false ^ " *" ^ var_name k)

let global_var_to_l (vs : VarSet.t) : string list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> "int " ^ var_name v)

(* Helper functions for making kernel variable declarations/prototypes *)
let arr_to_proto (vm : array_t VarMap.t) : PPrint.t list =
  VarMap.bindings vm 
  |> List.map (fun (k, v) -> PPrint.Line ("extern " ^ arr_type v true
                                          ^ " " ^ var_to_dummy k ^ "_w();"))

let arr_to_shared (vm : array_t VarMap.t) : PPrint.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> 
      PPrint.Line ((match v.array_size with | [] -> "extern " | _ -> "") 
                   ^ "__shared__ " ^ arr_type v false ^ " " ^ var_name k
                   ^ idx_to_s string_of_int v.array_size ^ ";"))

let arr_to_dummy (vm : array_t VarMap.t) : PPrint.t list =
  VarMap.bindings vm 
  |> List.map (fun (k, v) -> 
      PPrint.Line (arr_type v true ^ " " ^ var_to_dummy k ^ ";"))

let local_var_to_l (vs : VarSet.t) : PPrint.t list =
  (* A local variable must not be a tid/dummy variable *)
  let local_var = VarSet.filter
      (fun v -> not (VarSet.mem v thread_locals
                     || String.starts_with "__dummy" (var_name v))) vs
                  |> VarSet.elements in
  (* Generate a single prototype to initialize local variables with *)
  match local_var with
  | [] -> []
  | _ -> PPrint.Line "extern int __dummy_int();" :: List.map
           (fun v -> PPrint.Line ("int " ^ var_name v ^ " = __dummy_int();"))
           local_var

let body_to_s (f : 'a -> PPrint.t list) (k : 'a kernel) : PPrint.t =
  let shared_arr = arr_to_shared (VarMap.filter (fun k -> fun v ->
      v.array_hierarchy = SharedMemory) k.kernel_arrays) in
  let funct_protos = arr_to_proto k.kernel_arrays in
  let local_var = local_var_to_l k.kernel_local_variables in
  let dummy_var = arr_to_dummy k.kernel_arrays in
  PPrint.Block (shared_arr @ funct_protos @ local_var @ dummy_var
                @ (f k.kernel_code))

(* Serialization of the kernel *)
let kernel_to_s (f : 'a -> PPrint.t list) (k : 'a kernel) : PPrint.t list =
  let open PPrint in
  let decl_types = declare_unknown_types k.kernel_arrays in
  let k_name = if k.kernel_name = "main" then "kernel" else k.kernel_name in
  let global_arr = global_arr_to_l (VarMap.filter (fun k -> fun v ->
      v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let global_var = global_var_to_l k.kernel_global_variables in
  [
    decl_types;
    Line "__global__";
    Line ("void " ^ k_name ^ "(" ^ Common.join
            ", " (global_arr @ global_var) ^ ")");
    Line "{";
    (body_to_s f k);
    Line "}"
  ]

let print_kernel (f : 'a -> PPrint.t list) (k : 'a kernel) : unit =
  PPrint.print_doc (kernel_to_s f k)

let print_k (k : prog kernel) : unit =
  PPrint.print_doc (kernel_to_s prog_to_s k)

(* Kernel to TOML conversion *)
let arrays_to_l (vm : array_t VarMap.t) : Toml.Types.table list =
  let open Toml.Min in
  let open Toml.Types in
  VarMap.bindings vm
  |> List.map (fun (k, v) ->
      of_key_values [key (var_name k), TString (arr_type v false)])

let scalars_to_l (vs : VarSet.t) : Toml.Types.table list =
  let open Toml.Min in
  let open Toml.Types in
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> of_key_values [key (var_name v), TString "int"])

let kernel_to_toml (k : prog kernel) : Toml.Types.table =
  let global_arr = (VarMap.filter (fun k -> fun v ->
      v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let open Toml.Min in
  let open Toml.Types in
  let open PPrint in
  [
    key "pass", TBool true;
    key "includes", TArray (NodeInt []);
    key "body", TString (doc_to_string (Line "" :: [body_to_s prog_to_s k]));
    key "arrays", TArray (NodeTable (arrays_to_l global_arr));
    key "scalars", TArray (NodeTable (scalars_to_l k.kernel_global_variables));
  ]
  |> of_key_values

let print_toml (table : Toml.Types.table) : unit =
  print_string (Toml.Printer.string_of_table table)
