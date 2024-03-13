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
  only_array:string option;
  thread_idx_1: Dim3.t option;
  thread_idx_2: Dim3.t option;
  block_idx_1: Dim3.t option;
  block_idx_2: Dim3.t option;
  arch: Architecture.t;
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
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
  match app with
  | {filename; kernels; timeout; show_proofs; show_proto; show_wf;
     show_align; show_phase_split; show_loc_split; show_flat_acc;
     show_symbexp; logic; le_index = _; ge_index = _; eq_index = _;
     only_array = _; thread_idx_1 = _; block_idx_1 = _; thread_idx_2 = _;
     block_idx_2 = _; arch; block_dim; grid_dim; } ->
    let kernels = List.length kernels |> string_of_int in
    "filename: " ^ filename ^
    "\nblock_dim: " ^ dim3 block_dim ^
    "\ngrid_dim: " ^ dim3 grid_dim ^
    "\nkernels: " ^ kernels ^
    "\ntimeout: " ^ opt_int timeout ^
    "\nlogic: " ^ opt logic ^
    "\narch: " ^ Architecture.to_string arch ^
    "\nshow_proofs: " ^ bool show_proofs ^
    "\nshow_proto: " ^ bool show_proto ^
    "\nshow_wf: " ^ bool show_wf ^
    "\nshow_align: " ^ bool show_align ^
    "\nshow_phase_split: " ^ bool show_phase_split ^
    "\nshow_loc_split: " ^ bool show_loc_split ^
    "\nshow_flat_acc: " ^ bool show_flat_acc ^
    "\nshow_symbexp: " ^ bool show_symbexp ^
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
  ~arch
  ~ignore_parsing_errors
:
  t
=
  let set_block_idx
    (bid: Dim3.t)
    (ks: Proto.Code.t Proto.Kernel.t list)
  :
    Proto.Code.t Proto.Kernel.t list
  =
    let kvs = Dim3.to_assoc ~prefix:"blockIdx." bid in
    List.map (Proto.Kernel.assign_globals kvs) ks
  in
  let parsed = Protocol_parser.Silent.to_proto
    ~abort_on_parsing_failure:(not ignore_parsing_errors)
    ~includes
    ~block_dim
    ~grid_dim
    ~inline_calls
    filename
  in
  let kernels = parsed.kernels in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  (* Assign bid *)
  let kernels =
    match block_idx_1, arch with
    | Some block_idx_1, Architecture.Block ->
      set_block_idx block_idx_1 kernels
    | _, _ -> kernels
  in
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
    arch;
    grid_dim;
    block_dim;
  }

let show (b:bool) (call:'a -> unit) (x:'a) : 'a =
  if b then call x else ();
  x

let translate (a:t) (p:Proto.Code.t Proto.Kernel.t) : Flatacc.Kernel.t Streamutil.stream =
  p
  (* 1. apply block-level/grid-level analysis constraints *)
  (* filter arrays *)
  |> (fun k ->
    match a.only_array with
    | Some arr -> Proto.Kernel.filter_array arr k
    | None -> k
  )
  |> Proto.Kernel.apply_arch a.arch
  (* 2. inline global assignments, including block_dim/grid_dim *)
  |> Proto.Kernel.inline_all ~grid_dim:a.grid_dim ~block_dim:a.block_dim
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
  |> Flatacc.translate a.arch
  |> show a.show_flat_acc Flatacc.print_kernels

let check_unreachable (a:t) : unit =
  a.kernels
  |> List.iter (fun kernel ->
    let report =
      kernel
      |> translate a
      |> Symbexp.sanity_check a.arch
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
  let bid1, bid2 =
    if Architecture.is_grid a.arch then
      (a.block_idx_1, a.block_idx_2)
    else
      (* block idx is ignored in other levels,
          because it's being handled elsewhere *)
      (None, None)
  in
  a.kernels
  |> List.map (fun kernel ->
      let report =
      kernel
      |> translate a
      |> Symbexp.translate a.arch
      |> Symbexp.add_rel_index Exp.NLe a.le_index
      |> Symbexp.add_rel_index Exp.NGe a.ge_index
      |> Symbexp.add_rel_index Exp.NEq a.eq_index
      |> Symbexp.add ~tid:a.thread_idx_1 ~bid:bid1
      |> Symbexp.add ~tid:a.thread_idx_2 ~bid:bid2
      |> show a.show_symbexp Symbexp.print_kernels
      |> Z3_solver.Solution.solve
          ~timeout:a.timeout
          ~_show_proofs:a.show_proofs
          ~logic:a.logic
      |> Streamutil.to_list
    in
    Stdlib.flush_all ();
    Analysis.{kernel; report}
  )

