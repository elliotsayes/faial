open Proto
open Serialize
open Common
open Exp

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

(* Template kernel for random code generation *)
let kernel (p : prog) : prog kernel =
  {kernel_name = "kernel";
  kernel_global_variables = VarSet.of_list [ub];
  kernel_local_variables = VarSet.of_list [tid];
  kernel_arrays = mk_array_map GlobalMemory [arr];
  kernel_pre = distinct [tid];
  kernel_code = p}

let test_kernel = kernel (sync @@ loop2 @@ read @@ write [])

(* Helper functions for making the kernel header/parameters *)
let global_arr_to_tlist (vs:array_t VarMap.t) : Toml.Types.table list =
  VarMap.bindings vs
  |> List.map (fun (k,v) -> Toml.Min.of_key_values [Toml.Min.key (var_name k), Toml.Types.TString (Cgen.arr_type v)])

let global_var_to_tlist (vs:VarSet.t) : Toml.Types.table list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> Toml.Min.of_key_values [Toml.Min.key (var_name v), Toml.Types.TString "int"])

let kernel_to_toml (k:prog kernel) =
  let global_arr = (VarMap.filter (fun k -> fun v ->
  v.array_hierarchy = GlobalMemory) k.kernel_arrays) in
  let open Toml.Min in
  let open Toml.Types in
  of_key_values
  [
    key "pass", TBool true;
    key "includes", TArray (NodeInt []);
    key "body", TString ("\n" ^ PPrint.doc_to_string [PPrint.Block (Cgen.prog_to_s k.kernel_code)]);
    key "arrays", TArray (NodeTable (global_arr_to_tlist global_arr));
    key "scalars", TArray (NodeTable (global_var_to_tlist k.kernel_global_variables));
  ]

let () = print_string (Toml.Printer.string_of_table (kernel_to_toml test_kernel))

(* Generate all combinations *)

(* let () = 
  List.iter (fun p -> print_newline (Cgen.print_k (kernel p)))
    (List.map program_k (Common.range 1 (List.length program)) |> List.concat) *)
