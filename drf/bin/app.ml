open Stage0
open Protocols
open Drf
open Inference

type t = {
  filename: string;
  kernels: Proto.Code.t Proto.Kernel.t list;
  timeout:int option;
  show_proofs:bool;
  show_proto:bool;
  show_wf:bool;
  show_align:bool;
  show_phase_split:bool;
  show_loc_split:bool;
  show_flat_acc:bool;
  show_symbexp:bool;
  logic:string option;
  le_index:int list;
  ge_index:int list;
  eq_index:int list;
  only_kernel: string option;
  only_array:string option;
  thread_idx_1: Dim3.t option;
  thread_idx_2: Dim3.t option;
  block_idx_1: Dim3.t option;
  block_idx_2: Dim3.t option;
  archs: Architecture.t list;
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  params: (string * int) list;
  macros: string list;
}

let to_string (app:t) : string =
  let opt (o: string option) : string =
    Option.value ~default:"null" o
  in
  let opt_int (o:int option) : string =
    o
    |> Option.map string_of_int
    |> opt
  in
  let bool (b:bool) : string =
    if b then "true" else "false"
  in
  let dim3 (o:Dim3.t) : string =
    Dim3.to_string o
  in

  let list_string (l:string list) : string =
    "[" ^ (String.concat ", " l) ^ "]"
  in

  let list_arch (l:Architecture.t list) : string =
    list_string (List.map Architecture.to_string l)
  in
  match app with
  | {filename; kernels; timeout; show_proofs; show_proto; show_wf;
     show_align; show_phase_split; show_loc_split; show_flat_acc;
     show_symbexp; logic; le_index = _; ge_index = _; eq_index = _;
     only_array = _; thread_idx_1 = _; block_idx_1 = _; thread_idx_2 = _;
     block_idx_2 = _; archs; block_dim; grid_dim; params = _;
     only_kernel; macros; } ->
    let only_kernel = Option.value ~default:"(null)" only_kernel in
    let kernels = List.length kernels |> string_of_int in
    "filename: " ^ filename ^
    "\nonly_kernel: " ^ only_kernel ^
    "\nblock_dim: " ^ dim3 block_dim ^
    "\ngrid_dim: " ^ dim3 grid_dim ^
    "\nkernels: " ^ kernels ^
    "\ntimeout: " ^ opt_int timeout ^
    "\nlogic: " ^ opt logic ^
    "\narchs: " ^ list_arch archs ^
    "\nshow_proofs: " ^ bool show_proofs ^
    "\nshow_proto: " ^ bool show_proto ^
    "\nshow_wf: " ^ bool show_wf ^
    "\nshow_align: " ^ bool show_align ^
    "\nshow_phase_split: " ^ bool show_phase_split ^
    "\nshow_loc_split: " ^ bool show_loc_split ^
    "\nshow_flat_acc: " ^ bool show_flat_acc ^
    "\nshow_symbexp: " ^ bool show_symbexp ^
    "\nmacros = " ^ list_string macros ^
    "\n"

let parse
  ~filename
  ~timeout
  ~show_proofs
  ~show_proto
  ~show_wf
  ~show_align
  ~show_phase_split
  ~show_loc_split
  ~show_flat_acc
  ~show_symbexp
  ~logic
  ~ge_index
  ~le_index
  ~eq_index
  ~only_array
  ~thread_idx_1
  ~thread_idx_2
  ~block_idx_1
  ~block_idx_2
  ~block_dim
  ~grid_dim
  ~includes
  ~inline_calls
  ~archs
  ~ignore_parsing_errors
  ~params
  ~only_kernel
  ~macros
  ~cu_to_json
:
  t
=
  let parsed = Protocol_parser.Silent.to_proto
    ~abort_on_parsing_failure:(not ignore_parsing_errors)
    ~includes
    ~block_dim
    ~grid_dim
    ~inline_calls
    ~macros
    ~cu_to_json
    filename
  in
  let kernels = parsed.kernels in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  {
    filename;
    timeout;
    show_proofs;
    show_proto;
    show_wf;
    show_align;
    show_phase_split;
    show_loc_split;
    show_flat_acc;
    show_symbexp;
    logic;
    kernels;
    ge_index;
    le_index;
    eq_index;
    only_array;
    thread_idx_1;
    thread_idx_2;
    block_idx_1;
    block_idx_2;
    archs;
    grid_dim;
    block_dim;
    params;
    only_kernel;
    macros;
  }

let show (b:bool) (call:'a -> unit) (x:'a) : 'a =
  if b then call x else ();
  x

let translate (arch:Architecture.t) (a:t) (k:Proto.Code.t Proto.Kernel.t) : Flatacc.Kernel.t Streamutil.stream =
  k
  (* 0. filter arrays *)
  |> (fun k ->
    match a.only_array with
    | Some arr -> Proto.Kernel.filter_array arr k
    | None -> k
  )
  (* 1. apply block-level/grid-level analysis constraints *)
  |> Proto.Kernel.apply_arch arch
  (* 2. inline global assignments, including block_dim/grid_dim *)
  |> Proto.Kernel.inline_all
    ~grid_dim:a.grid_dim
    ~block_dim:a.block_dim
    ~globals:a.params
  (* 2.1 inline block_id as a constant when architecture is Grid *)
  |> (fun k ->
    match arch, a.block_idx_1 with
    | Architecture.Block, Some bid ->
      let kvs = Dim3.to_assoc ~prefix:"blockIdx." bid in
      Proto.Kernel.assign_globals kvs k
    | _, _ ->
      k
  )
  |> show a.show_proto (Proto.Kernel.print Proto.Code.to_s)
  (* 3. constant folding optimization *)
  |> Proto.Kernel.opt
  (* 4. convert to well-formed protocol *)
  |> Wellformed.translate
  |> show a.show_wf Wellformed.print_kernels
  (* 5. align protocol *)
  |> Aligned.translate
  |> show a.show_align Aligned.print_kernels
  (* 6. split per sync *)
  |> Phasesplit.translate
  |> show a.show_phase_split Phasesplit.print_kernels
  (* 7. split per location *)
  |> Locsplit.translate
  |> show a.show_loc_split Locsplit.print_kernels
  (* 8. flatten control-flow structures *)
  |> Flatacc.translate arch
  |> show a.show_flat_acc Flatacc.print_kernels

let only_kernel
  (a:t)
  (ks:Proto.Code.t Proto.Kernel.t list)
:
  Proto.Code.t Proto.Kernel.t list
=
  match a.only_kernel with
  | Some name ->
    let ks =
      ks
      |> List.filter (fun k -> let open Proto.Kernel in k.name = name)
    in
    if ks = [] then
      (Logger.Colors.error ("kernel '" ^ name ^ "' not found!");
        exit (-1)
      )
    else ks
  | None -> ks

let check_unreachable (a:t) : unit =
  a.kernels
  |> only_kernel a
  |> List.iter (fun kernel ->
    let report =
      kernel
      |> translate Architecture.Block a
      |> Symbexp.sanity_check Architecture.Block
      |> show a.show_symbexp Symbexp.print_kernels
      |> Streamutil.map (fun b ->
        (b, Z3_solver.solve ~timeout:a.timeout ~logic:a.logic b)
      )
      |> Streamutil.to_list
    in
    Stdlib.flush_all ();
    report |> List.iter (fun (p, s) ->
      let open Z3.Solver in
      match s with
      | UNSATISFIABLE | UNKNOWN ->
        Symbexp.Proof.to_string p |> print_endline
      | SATISFIABLE -> ()
    )
  )



let run (a:t) : Analysis.t list =
  let check_kernel (arch) (kernel:Proto.Code.t Proto.Kernel.t) : Analysis.t =
    let report =
      kernel
      |> translate arch a
      |> Symbexp.translate arch
      |> Symbexp.add_rel_index Exp.NLe a.le_index
      |> Symbexp.add_rel_index Exp.NGe a.ge_index
      |> Symbexp.add_rel_index Exp.NEq a.eq_index
      |> Symbexp.add ~tid:a.thread_idx_1 ~bid:a.block_idx_1
      |> Symbexp.add ~tid:a.thread_idx_2 ~bid:a.block_idx_2
      |> show a.show_symbexp Symbexp.print_kernels
      |> Z3_solver.Solution.solve
          ~timeout:a.timeout
          ~_show_proofs:a.show_proofs
          ~logic:a.logic
      |> Streamutil.to_list
    in
    Analysis.{kernel; report}
  in
  a.kernels
  |> only_kernel a
  |> List.map (fun kernel ->
    let rec check_until (archs:Architecture.t list) : Analysis.t =
      match archs with
      | [] -> Analysis.{kernel; report=[]}
      | [arch] -> check_kernel arch kernel
      | arch::archs ->
        let a = check_kernel arch kernel in
        if Analysis.is_safe a then
          check_until archs
        else
          a
    in
    check_until a.archs
  )

