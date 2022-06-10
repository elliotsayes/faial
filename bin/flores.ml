open Proto
open Serialize
open Common
open Exp
open Cgen

let arr = var_make "A"
let ub = var_make "n"
let tid = var_make "threadIdx.x"

let thread_globals : VarSet.t =
  VarSet.of_list (List.map var_make
  ["gridDim.x"; "blockIdx.x"; "blockDim.x";
  "gridDim.y"; "blockIdx.y"; "blockDim.y";
  "gridDim.z"; "blockIdx.z"; "blockDim.z"])

let write (p : prog) : prog =
  p @ [Acc (arr, {access_index = [Var tid]; access_mode = W})]

let read (p : prog) : prog =
  p @ [Acc (arr, {access_index = [Var tid]; access_mode = R})]

let sync (p : prog) : prog =
  p @ [Sync]

(* Inner loop *)
let loop1 (p : prog) : prog =
  [Loop (mk_range (var_make "x") (Var ub), p)]

(* Outer loop *)
let loop2 (p : prog) : prog =
  p @ [Loop (mk_range (var_make "x") (Var ub), [])]

(* Inner conditional *)
let cond1 (p : prog) : prog =
  [Cond (NRel (Exp.NEq, Var tid, Num 0), p)]

(* Outer conditional *)
let cond2 (p : prog) : prog =
  p @ [Cond (NRel (Exp.NEq, Var tid, Num 0), [])]

let rec choose (k : int) (l : ('a list -> 'a list) list) : 'a list list =
  if k <= 0 then [[]]
  else match l with
  | [] -> []
  | h :: t ->
    let with_h = List.map (fun l -> h l) (choose (k - 1) t) in
    let without_h = choose k t in
  with_h @ without_h

let program = [sync; loop1; loop2; cond1; cond2; read; write]

let program_k (k : int) : prog list = choose k program

let kernel (p : prog) : prog kernel =
  {kernel_name = "kernel";
  kernel_global_variables = VarSet.of_list [ub];
  kernel_local_variables = VarSet.of_list [tid];
  kernel_arrays = mk_array_map GlobalMemory [arr];
  kernel_pre = distinct [tid];
  kernel_code = p}

let test_kernel = kernel (sync @@ loop2 @@ read @@ write [])

let array_map_to_s (vs:array_t VarMap.t) : string =
  VarMap.bindings vs
  |> List.map (fun (k, v) -> var_name k)
  |> Common.join ", "

let var_set_to_s (vs:VarSet.t) (prefix:string) : string =
  VarSet.elements vs
  |> List.map (fun v -> prefix ^ var_name v)
  |> Common.join ",\n"

let kernel_to_s (f:'a -> PPrint.t list) (k:'a kernel) : PPrint.t list =
  [
    Line ("shared " ^ array_map_to_s k.kernel_arrays ^ ";\n");
    Line ("const \n" ^ (var_set_to_s k.kernel_global_variables "  global ")
                     ^ (var_set_to_s k.kernel_local_variables "  local "));
    Line ("  where " ^ PPrint.b_to_s k.kernel_pre ^ ";\n");
    Line (PPrint.doc_to_string (f k.kernel_code));
  ]

let print_kernel (f:'a -> PPrint.t list) (k: 'a kernel) : unit =
  PPrint.print_doc (kernel_to_s f k)

let print_k (k:prog kernel) : unit =
  PPrint.print_doc (kernel_to_s prog_to_s k)

(* Get the type of an array, defaulting to int if it is unknown *)
let arr_type (arr:array_t) : string =
  match arr.array_type with
  | [] -> "int"
  | _ -> Common.join " " arr.array_type

(* Helper functions for making the kernel header/parameters *)
let global_arr_to_l (vs:array_t VarMap.t) =
  VarMap.bindings vs
  |> List.map (fun (k,v) -> Toml.Min.of_key_values [Toml.Min.key (var_name k), Toml.Types.TString (arr_type v)])

let global_var_to_l (vs:VarSet.t) =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> Toml.Min.of_key_values [Toml.Min.key (var_name v), Toml.Types.TString "int"])

let kernel_to_toml (k:prog kernel) =
  let arrays = (VarMap.partition (fun k -> fun v ->
  v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  Toml.Min.of_key_values [
    (* Toml.Min.key "grid_dim", Toml.Types.TArray (Toml.Types.NodeInt [64; 1;]);
    Toml.Min.key "block_dim", Toml.Types.TArray (Toml.Types.NodeInt [128; 1;]); *)
    Toml.Min.key "pass", Toml.Types.TBool true;
    Toml.Min.key "includes", Toml.Types.TArray (Toml.Types.NodeInt []);
    (* Toml.Min.key "pre", Toml.Types.TArray (Toml.Types.NodeString [PPrint.b_to_s k.kernel_pre]); *)
    Toml.Min.key "body", Toml.Types.TString ("\n" ^ PPrint.doc_to_string [PPrint.Block (prog_to_s k.kernel_code)]);
    Toml.Min.key "arrays", Toml.Types.TArray (Toml.Types.NodeTable (global_arr_to_l (fst arrays)));
    Toml.Min.key "scalars", Toml.Types.TArray (Toml.Types.NodeTable (global_var_to_l k.kernel_global_variables));
]

let () = print_string (Toml.Printer.string_of_table (kernel_to_toml test_kernel))

(* let () = Codegen.print_k test_kernel *)

(* let () = print_k (test_kernel) *)

(* Generate all combinations *)

(* let () = 
  List.iter (fun p -> print_newline (print_k (kernel p)))
    (List.map program_k (Common.range 1 (List.length program)) |> List.concat) *)
