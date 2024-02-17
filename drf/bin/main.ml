open Stage0
open Protocols
open Inference
open Drf

module Environ = Z3_solver.Environ
module Witness = Z3_solver.Witness
module Vec3 = Z3_solver.Vec3
module Task = Z3_solver.Task
module T = ANSITerminal

let box_environ (e:Environ.t) : PrintBox.t =
  PrintBox.(
    v_record (List.map (fun (k,v) -> (k, text v)) (Environ.labels e))
    |> frame
  )

let struct_to_s (l:(string * string) list) : string =
  l
  |> List.map (fun (key, elem) -> (key ^ " = " ^ elem))
  |> Common.join " | "

let dim_to_s (v: Vec3.t) : string =
  ["x", v.x; "y", v.y; "z", v.z]
  |> List.filter (fun (_, v) -> v <> "1")
  |> struct_to_s


let idx_to_s ~idx ~dim =
  let pos_fields =
    Vec3.to_assoc dim
    |> List.filter_map (fun (k, v) -> if v = "1" then None else Some k)
  in
  idx
  |> Vec3.to_assoc
  |> List.filter (fun (k, _) -> List.mem k pos_fields)
  |> struct_to_s


let box_idx key ~idx ~dim =
  let idx = idx_to_s ~idx ~dim in
  if idx = "" then []
  else [key, idx]


let box_tasks ~data_approx ~control_approx (block_dim:Vec3.t) (t1:Task.t) (t2:Task.t) : PrintBox.t =
  let open PrintBox in
  let locals =
    t1.locals
    |> Environ.variables
    |> List.map (fun (k, v1) ->
      let lbl = Option.value ~default:k (Environ.label k t1.locals) in
      let is_dd = Variable.Set.mem (Variable.from_name k) data_approx in
      let is_cd = Variable.Set.mem (Variable.from_name k) control_approx in
      let is_approx = is_cd || is_dd in
      let style = if is_approx then Style.bold else Style.default in
      let lbl =
        if is_approx then
          let msg = if is_cd then "C" else "" in
          let msg = msg ^ (if is_dd then "D" else "") in
          lbl ^ " (" ^ msg ^ ")"
        else
          lbl
      in
      [|
        text_with_style style lbl;
        text v1;
        text (Environ.get k t2.locals |> Option.value ~default:"?")
      |]
    )
  in
  let locals =
    [|
      text "threadIdx";
      text @@ idx_to_s ~idx:t1.thread_idx ~dim:block_dim;
      text @@ idx_to_s ~idx:t2.thread_idx ~dim:block_dim;
    |] :: locals
    |> Array.of_list
  in
  grid locals |> frame

let box_globals (w:Witness.t) : PrintBox.t =
  let dim x = if x = "1" then 0 else 1 in
  let dim_len v =
    let open Vec3 in
    dim v.x + dim v.y + dim v.z
  in
  let box_dim name v =
    if dim_len v = 0 then []
    else
      [name, dim_to_s v]
  in
  { w.globals with
  variables=
  [
    "index", Common.join " │ " w.indices;
  ]
  @ box_dim "gridDim" w.grid_dim
  @ box_idx "blockIdx" ~idx:w.block_idx ~dim:w.grid_dim
  @ box_dim "blockDim" w.block_dim
  @ w.globals.variables
  }
  |> box_environ

let box_locals (w:Witness.t) : PrintBox.t =
  let (t1, t2) = w.tasks in
  box_tasks
    ~data_approx:w.data_approx
    ~control_approx:w.control_approx
    w.block_dim t1 t2

let print_box: PrintBox.t -> unit =
  PrintBox_text.output stdout

module Analysis = struct
  type t = {
    kernel: Proto.Code.t Proto.Kernel.t;
    report: (Symbexp.Proof.t * Z3_solver.Solution.t) list;
  }
end

module App = struct
  type t = {
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
    block_dim:Vec3.t;
    grid_dim:Vec3.t;
    le_index:int list;
    ge_index:int list;
    eq_index:int list;
    only_array:string option;
    thread_idx_1: Dim3.t option;
    thread_idx_2: Dim3.t option;
  }
  let make
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
    ~block_dim
    ~grid_dim
    ~ge_index
    ~le_index
    ~eq_index
    ~only_array
    ~thread_idx_1
    ~thread_idx_2
    (kernels: Proto.Code.t Proto.Kernel.t list)
  :
    t
  =
    (* We need to ensure that thread_idx_1 is GREATER THAN thread_idx_2,
       otherwise, when they are both defined the matching will not find
       a data-race. *)
    let thread_idx_1, thread_idx_2 =
      if Option.compare Dim3.compare thread_idx_1 thread_idx_2 < 0 then
        (thread_idx_2, thread_idx_1)
      else
        (thread_idx_1, thread_idx_2)
    in
    {
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
      block_dim;
      grid_dim;
      kernels;
      ge_index;
      le_index;
      eq_index;
      only_array;
      thread_idx_1;
      thread_idx_2;
    }

  let run (a:t) : Analysis.t list =
    a.kernels
    |> List.map (fun p ->
      let kernel = p in
      let show (b:bool) (call:unit -> unit) : unit =
        if b then call () else ()
      in
      show a.show_proto (fun () -> Proto.Kernel.print Proto.Code.to_s p);
      let p = p |> Proto.Kernel.opt |> Wellformed.translate in
      show a.show_wf (fun () -> Wellformed.print_kernels p);
      let p = Aligned.translate p in
      show a.show_align (fun () -> Aligned.print_kernels p);
      let p = Phasesplit.translate p false in
      show a.show_phase_split (fun () -> Phasesplit.print_kernels p);
      let p = Locsplit.translate p |> Locsplit.filter_array a.only_array in
      show a.show_loc_split (fun () -> Locsplit.print_kernels p);
      let p = Flatacc.translate p in
      show a.show_flat_acc (fun () -> Flatacc.print_kernels p);
      let p =
        p
        |> Symbexp.translate
        |> Symbexp.add_rel_index Exp.NLe a.le_index
        |> Symbexp.add_rel_index Exp.NGe a.ge_index
        |> Symbexp.add_rel_index Exp.NEq a.eq_index
        |> Symbexp.add_tid Task1 a.thread_idx_1
        |> Symbexp.add_tid Task2 a.thread_idx_2
      in
      show a.show_symbexp (fun () -> Symbexp.print_kernels p);
      let open Z3_solver in
      let open Solution in
      let report =
        p
        |> solve
            ~grid_dim:(Some a.grid_dim)
            ~block_dim:(Some a.block_dim)
            ~timeout:a.timeout
            ~show_proofs:a.show_proofs
            ~logic:a.logic
        |> Streamutil.to_list
      in
      Stdlib.flush_all ();
      Analysis.{kernel; report}
    )


end

let tui (output: Analysis.t list) : unit =
  output
  |> List.iter (fun solution ->
    let kernel_name =
      let open Analysis in
      solution.kernel.name in
    let errors =
      solution.report
      |> List.filter_map (
        let open Z3_solver.Solution in
        function
        | _, Drf -> None
        | p, Unknown -> Some (Either.Left p)
        | p, Racy w -> Some (Either.Right (p, w))
      )
    in
    let print_errors errs =
      errs |> List.iteri (fun i (w:Witness.t) ->
        let is_cd = Variable.Set.cardinal w.control_approx > 0 in
        let is_dd = Variable.Set.cardinal w.data_approx > 0 in
        let is_exact = not is_cd && not is_dd in
        let lbl = " (" ^
          (if is_cd then "CD" else "CI") ^
          (if is_dd then "DD" else "DI") ^
          ")"
        in
        T.print_string [T.Bold; T.Foreground T.Blue] ("\n~~~~ Data-race " ^ string_of_int (i + 1) ^ lbl ^ " ~~~~\n\n");
        let (t1, t2) = w.tasks in
        let locs =
          let l = [t1.location; t2.location] |> Common.flatten_opt in
          match l with
          | [x1; x2] when x1 = x2 -> [x1]
          | [x1; x2] when x2 < x1 -> [x2; x1]
          | _ -> l
        in
        (match locs with
        | [x] -> Tui.LocationUI.print x
        | [x1; x2] -> Tui.LocationUI.print2 x1 x2
        | _ -> failwith "??"
        );
        print_endline "";
        T.print_string [T.Bold] ("Globals\n");
        box_globals w |> print_box;
        T.print_string [T.Bold] ("\n\nLocals\n");
        box_locals w |> print_box;
        (if is_exact then
          T.print_string [T.Bold; T.Underlined; T.Foreground T.Red] ("\nTrue alarm detected!\n")
        else
          ());
        (if is_dd then
          T.print_string [T.Bold; T.Underlined; T.Foreground T.Yellow] ("\nWARNING: potential alarm, index depends on input, see variables with (D).\n")
        else
          ());
        (if is_cd then
          (T.print_string [T.Bold; T.Underlined; T.Foreground T.Yellow] ("\nWARNING: potential alarm, control-flow depends on input, see variables with (C).\n");
          )
        else
          ());
        print_endline "";
        T.print_string [T.Underlined] ("(proof #" ^ string_of_int w.proof_id ^ ")\n");
      );
    in
    match Common.either_split errors with
    | [], [] ->
      T.print_string [T.Bold; T.Foreground T.Green] ("Kernel '" ^ kernel_name ^ "' is DRF!\n")
    | unk, errs ->
      let has_unknown = List.length unk > 0 in
      let errs = List.split errs |> snd in
      let err_count = List.length errs |> string_of_int in
      let dr = "data-race" ^ (if err_count = "1" then "" else "s") in
      T.print_string [T.Bold; T.Foreground T.Red] ("Kernel '" ^ kernel_name ^ "' has " ^ err_count ^ " " ^ dr ^ ".\n");
      print_errors errs;
      if has_unknown then
        T.print_string [T.Foreground T.Red] ("A portion of the kernel was not analyzable. Try to increasing the timeout.\n")
      else ();
      if err_count <> "0" || has_unknown then
        exit 1
      else
        ()
  )

let jui (output: Analysis.t list) : unit =
  let kernels =
    output
    |> List.map (fun analysis ->
      let open Analysis in
      let kernel_name = analysis.kernel.name in
      let solutions = analysis.report in
      let unknowns, errors =
        solutions
        |> List.filter_map (
          let open Z3_solver.Solution in
          function
          | _, Drf -> None
          | p, Unknown -> Some (Either.Left p)
          | p, Racy w -> Some (Either.Right (p, w))
        )
        |> Common.either_split
      in
      let approx_analysis (w:Witness.t) =
        let dd = if Variable.Set.cardinal w.data_approx > 0 then "DD" else "DI" in
        let cd = if Variable.Set.cardinal w.control_approx > 0 then "CD" else "CI" in
        cd ^ dd
      in
      let is_ok = (List.length unknowns + List.length errors) = 0 in
      `Assoc [
        "kernel_name", `String kernel_name;
        "status", `String (if is_ok then "drf" else "racy");
        "unknowns", `List (List.map Symbexp.Proof.to_json unknowns);
        "errors", `List (List.map (fun (p, w) ->
          `Assoc [
            "summary", Symbexp.Proof.to_json p;
            "counter_example", Witness.to_json w;
            "approx_analysis", `String (approx_analysis w);
          ]
        ) errors);
      ]
    )
  in
  `Assoc [
    "kernels", `List kernels;
    "argv", `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x));
    "executable_name", `String Sys.executable_name;
    "z3_version", `String (Z3.Version.to_string);
  ]
  |> Yojson.Basic.to_string
  |> print_endline

let main
  (fname: string)
  (timeout:int option)
  (show_proofs:bool)
  (show_proto:bool)
  (show_wf:bool)
  (show_align:bool)
  (show_phase_split:bool)
  (show_loc_split:bool)
  (show_flat_acc:bool)
  (show_symbexp:bool)
  (logic:string option)
  (output_json:bool)
  (ignore_parsing_errors:bool)
  (block_dim:string option)
  (grid_dim:string option)
  (includes:string list)
  (ignore_calls:bool)
  (ge_index:int list)
  (le_index:int list)
  (eq_index:int list)
  (only_array:string option)
  (thread_idx_1:Dim3.t option)
  (thread_idx_2:Dim3.t option)
:
  unit
=
  let parsed = Protocol_parser.Silent.to_proto
    ~abort_on_parsing_failure:(not ignore_parsing_errors)
    ~includes
    ~inline:(not ignore_calls)
    fname
  in
  let parse_dim (given:string option) (parsed:Dim3.t) : Vec3.t =
    (match given with
    | Some x -> Dim3.parse x |> Result.value ~default:parsed
    | None -> parsed
    )
    |> Vec3.from_dim3
  in
  let block_dim = parse_dim block_dim parsed.options.block_dim in
  let grid_dim = parse_dim grid_dim parsed.options.grid_dim in
  let ui = if output_json then jui else tui in
  parsed.kernels
  |> App.make
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
      ~block_dim
      ~grid_dim
      ~ge_index
      ~le_index
      ~eq_index
      ~only_array
      ~thread_idx_1
      ~thread_idx_2
  |> App.run
  |> ui


open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let get_timeout =
  let doc = "Sets a timeout in millisecs. Default: $(docv)" in
  Arg.(value & opt (some int) None & info ["t"; "timeout"] ~docv:"MILISECS" ~doc)

let logic =
  let doc = "Set the logic used by the Z3 solver." in
  Arg.(value & opt (some string) None & info ["logic"] ~doc)

let show_proofs =
  let doc = "Show the Z3 proofs being generated." in
  Arg.(value & flag & info ["show-proofs"] ~doc)

let show_proto =
  let doc = "Show the MAP kernel." in
  Arg.(value & flag & info ["show-map"] ~doc)

let show_wf =
  let doc = "Show the well-formed kernel." in
  Arg.(value & flag & info ["show-well-formed"] ~doc)

let show_aligned =
  let doc = "Show the aligned kernel." in
  Arg.(value & flag & info ["show-aligned"] ~doc)

let show_phase_split =
  let doc = "Show the phase-split kernel." in
  Arg.(value & flag & info ["show-phase-split"] ~doc)

let show_loc_split =
  let doc = "Show the location-split kernel." in
  Arg.(value & flag & info ["show-loc-split"] ~doc)

let show_flat_acc =
  let doc = "Show the flat-access kernel." in
  Arg.(value & flag & info ["show-flat-acc"] ~doc)

let show_symbexp =
  let doc = "Show the symbexp kernel." in
  Arg.(value & flag & info ["show-symbexp"] ~doc)

let output_json =
  let doc = "Output result as JSON." in
  Arg.(value & flag & info ["json"] ~doc)

let ignore_parsing_errors =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info ["ignore-parsing-errors"] ~doc)

let dim_help = {|
  The value will be loaded from header if omitted.\
  Examples (without quotes): "[2,2,2]" or "32"
|}

let block_dim =
  let d = Gv_parser.default_block_dim |> Dim3.to_string in
  let doc = "Sets the number of threads per block." ^ dim_help ^ "Default: " ^ d in
  Arg.(value & opt (some string) None & info ["b"; "block-dim"; "blockDim"] ~docv:"DIM3" ~doc)

let grid_dim =
  let d = Gv_parser.default_grid_dim |> Dim3.to_string in
  let doc = "Sets the number of blocks per grid." ^ dim_help ^ "Default: " ^ d in
  Arg.(value & opt (some string) None & info ["g"; "grid-dim"; "gridDim"] ~docv:"DIM3" ~doc)

let conv_dim3 default =
  let parse =
    fun s ->
      match Dim3.parse ~default s with
      | Ok e -> `Ok e
      | Error e -> `Error e
  in
  let print _ (l:Dim3.t) : unit =
    Dim3.to_string l
    |> print_string
  in
  (parse, print)


let thread_idx_1 =
  let doc = "Sets the thread index for one thread." ^ dim_help ^ "Default: (none)" in
  Arg.(value & opt (some (conv_dim3 Dim3.zero)) None & info ["thread-idx-1"] ~docv:"DIM3" ~doc)

let thread_idx_2 =
  let doc = "Sets the thread index for another thread." ^ dim_help ^ "Default: (none)" in
  Arg.(value & opt (some (conv_dim3 Dim3.zero)) None & info ["thread-idx-2"] ~docv:"DIM3" ~doc)

let include_dir =
  let doc = "Add the specified directory to the search path for include files." in
  Arg.(value & opt_all string [] & info ["I"; "include-dir";] ~docv:"DIR" ~doc)

let ignore_calls =
  let doc = "By default we inline kernel calls, this option skips that step." in
  Arg.(value & flag & info ["ignore-calls"] ~doc)

let conv_int_list =
  let parse =
    fun s ->
      let msg = "Invalid JSON format: Expected a list of ints, but got: " ^ s in
      try
        (match Yojson.Basic.from_string s with
        | `List lst ->
            `Ok (List.map (function
              | `Int n -> n
              | _ -> failwith msg
            ) lst)
        | _ -> failwith msg)
      with _ ->
        `Error msg
  in
  let print _ (l:int list) : unit =
    "[" ^ (List.map string_of_int l |> String.concat ", ") ^ "]"
    |> print_string
  in
  (parse, print)

let eq_index =
  let doc = "Check that each index is greater-or-equal than the argument. Expects an integer, or a (JSON) list of integers. Example: 1 or [1,2]" in
  Arg.(value & opt conv_int_list [] & info ["index"] ~docv:"LIST" ~doc)

let ge_index =
  let doc = "Check that each index is greater-or-equal than the argument. Expects an integer, or a (JSON) list of integers. Example: 1 or [1,2]" in
  Arg.(value & opt conv_int_list [] & info ["ge-index"] ~docv:"LIST" ~doc)

let le_index =
  let doc = "Check that each index is lesser-or-equal than the argument. Expects an integer, or a (JSON) list of integers. Example: 1 or [1,2]" in
  Arg.(value & opt conv_int_list [] & info ["le-index"] ~docv:"LIST" ~doc)

let only_array =
  let doc = "Only check a specific array." in
  Arg.(value & opt (some string) None & info ["array"] ~doc)

let main_t = Term.(
  const main
  $ get_fname
  $ get_timeout
  $ show_proofs
  $ show_proto
  $ show_wf
  $ show_aligned
  $ show_phase_split
  $ show_loc_split
  $ show_flat_acc
  $ show_symbexp
  $ logic
  $ output_json
  $ ignore_parsing_errors
  $ block_dim
  $ grid_dim
  $ include_dir
  $ ignore_calls
  $ ge_index
  $ le_index
  $ eq_index
  $ only_array
  $ thread_idx_1
  $ thread_idx_2
)

let info =
  let doc = "Verify if CUDA file is free from data races." in
  Cmd.info "faial-drf" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

