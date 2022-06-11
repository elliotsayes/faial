open Exp
open Proto
open Common
open Serialize

(* ----------------- constants -------------------- *)
let thread_globals : VarSet.t =
  VarSet.of_list (List.map var_make
  ["gridDim.x"; "blockIdx.x"; "blockDim.x";
  "gridDim.y"; "blockIdx.y"; "blockDim.y";
  "gridDim.z"; "blockIdx.z"; "blockDim.z"])

(* ----------------- serialization -------------------- *)

(* Gives the dummy variable string for any variable *)
let var_to_dummy (v:variable) : string =
  "__dummy" ^ var_name v

(* Maps a list of values to an index-separated string containing them *)
let idx_to_s (f:'a -> string) (l:'a list) : string =
  "[" ^ (Common.join "][" (List.map f l)) ^ "]"
  
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

let prog_to_s (p: prog) : PPrint.t list =
  List.map inst_to_s p |> List.flatten

(* Get the type of an array, defaulting to int if it is unknown *)
let arr_type (arr:array_t) : string =
  match arr.array_type with
  | [] -> "int"
  | _ -> Common.join " " arr.array_type

(* Helper functions for making the kernel header/parameters *)
let global_arr_to_l (vs:array_t VarMap.t) : string list =
  VarMap.bindings vs
  |> List.map (fun (k,v) -> arr_type v ^ " *" ^ var_name k)

let global_var_to_l (vs:VarSet.t) : string list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> "int " ^ var_name v)

(* Helper functions for making kernel variable declarations/prototypes *)
let arr_to_proto (vs:array_t VarMap.t) : string list =
  VarMap.bindings vs
  |> List.map (fun (k,v) -> "    extern " ^
  arr_type v ^ " " ^ var_to_dummy k ^ "_w();")

let arr_to_shared (vs:array_t VarMap.t) : string list = 
  VarMap.bindings vs
  |> List.map (fun (k,v) -> "    " ^ (match v.array_size with
  | [] -> "extern " | _ -> "") ^ "__shared__ " ^ arr_type v ^ " " ^
  var_name k ^ idx_to_s string_of_int v.array_size ^ ";")

let arr_to_dummy (vs:array_t VarMap.t) : string list =
  VarMap.bindings vs
  |> List.map (fun (k,v) -> "    " ^ arr_type v ^ " " ^ var_to_dummy k ^ ";")
  

(* Serialization of the kernel *)
let kernel_to_s (f:'a -> PPrint.t list) (k:'a kernel) : PPrint.t list =
  let open PPrint in
  let arrays = (VarMap.partition (fun k -> fun v ->
    v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let global_arr = global_arr_to_l (fst arrays) in
  let global_var = global_var_to_l k.kernel_global_variables in
  let dummy_var = arr_to_dummy k.kernel_arrays in
  let shared_arr = arr_to_shared (snd arrays) in
  let funct_protos = arr_to_proto k.kernel_arrays in
  [
    Line "__global__";
    Line ("void " ^ k.kernel_name ^ "(" ^ Common.join
    ", " (global_arr @ global_var) ^ ")");
    Line "{";
    Line (Common.join "\n" (dummy_var @ shared_arr @ funct_protos));
    Block (f k.kernel_code);
    Line "}"
  ]
    
let print_kernel (f:'a -> PPrint.t list) (k: 'a kernel) : unit =
  PPrint.print_doc (kernel_to_s f k)

let print_k (k:prog kernel) : unit =
  PPrint.print_doc (kernel_to_s prog_to_s k)

(* Kernel to TOML conversion *)
let global_arr_to_tlist (vs:array_t VarMap.t) : Toml.Types.table list =
  VarMap.bindings vs
  |> List.map (fun (k,v) -> Toml.Min.of_key_values [Toml.Min.key (var_name k), Toml.Types.TString (arr_type v)])

let global_var_to_tlist (vs:VarSet.t) : Toml.Types.table list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> Toml.Min.of_key_values [Toml.Min.key (var_name v), Toml.Types.TString "int"])

let kernel_to_toml (k:prog kernel) : Toml.Types.table =
  let global_arr = (VarMap.filter (fun k -> fun v ->
  v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let open Toml.Min in
  let open Toml.Types in
  of_key_values
  [
    key "pass", TBool true;
    key "includes", TArray (NodeInt []);
    key "body", TString (PPrint.doc_to_string (kernel_to_s prog_to_s k));
    key "arrays", TArray (NodeTable (global_arr_to_tlist global_arr));
    key "scalars", TArray (NodeTable (global_var_to_tlist k.kernel_global_variables));
  ]

let print_toml (toml:Toml.Types.table) : unit =
  print_string (Toml.Printer.string_of_table toml)
