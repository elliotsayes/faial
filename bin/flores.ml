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

let () = print_string (Toml.Printer.string_of_table (Cgen.kernel_to_toml test_kernel))

(* Generate all combinations *)

(* let () = 
  List.iter (fun p -> print_newline (Cgen.print_k (kernel p)))
    (List.map program_k (Common.range 1 (List.length program)) |> List.concat) *)
