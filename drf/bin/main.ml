open Stage0
open Protocols
open Inference
open Drf

module Environ = Z3_solver.Environ
module Witness = Z3_solver.Witness
module StringMap = Common.StringMap
module Task = Z3_solver.Task
module T = ANSITerminal

let box_environ (e:Environ.t) : PrintBox.t =
  PrintBox.(
    v_record (List.map (fun (k,v) -> (k, text v)) (Environ.labels e))
    |> frame
  )



module LocalState = struct
  type t = {
    control_dependent: bool;
    data_dependent: bool;
    state: string * string;
  }

  let parse_structs
    ~data_approx:(data_approx:Variable.Set.t)
    ~control_approx:(control_approx:Variable.Set.t)
    ~t1:(t1:Task.t)
    ~t2:(t2:Task.t)
  :
    (string * t) list
  =
    let t1_s = Environ.parse_structs t1.locals in
    let t2_s = Environ.parse_structs t2.locals in
    (* Merge both maths, so we get identifier to struct, where a struct
       is a map from field to value. A value is a pair for each thread's local
       state *)
    StringMap.merge (fun _ v1 v2 ->
    match v1, v2 with
    | Some v, None -> Some (v, StringMap.empty)
    | None, Some v -> Some (StringMap.empty, v)
    | Some v1, Some v2 -> Some (v1, v2)
    | None, None -> None
    ) t1_s t2_s
    |> StringMap.bindings
    (* At this point we have the map of structs *)
    |> List.map (fun (ident, (s1, s2)) ->
      (* Calculuate the local variables, of a particular struct *)
      let vars (s:string StringMap.t) : Variable.Set.t =
        s
        |> StringMap.bindings
        |> List.map (fun (field, _) ->
          ident ^ "." ^ field
          |> Variable.from_name
        )
        |> Variable.Set.of_list
      in
      let all_vars = Variable.Set.union (vars s1) (vars s2) in
      let is_in (vs:Variable.Set.t) : bool =
        Variable.Set.cardinal (Variable.Set.inter vs all_vars) > 0
      in
      let to_string s =
        s
        |> StringMap.bindings
        |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
        |> List.map (fun (f, v) ->
          f ^ " = " ^ v
        )
        |> String.concat " | "
      in
      (ident, {
        control_dependent = is_in control_approx;
        data_dependent = is_in data_approx;
        state=(to_string s1, to_string s2)
      })
    )

  let parse_scalars
    ~data_approx:(data_approx:Variable.Set.t)
    ~control_approx:(control_approx:Variable.Set.t)
    ~t1:(t1:Task.t)
    ~t2:(t2:Task.t)
  :
    (string * t) list
  =
    t1.locals
    |> Environ.remove_structs
    |> Environ.variables
    |> List.map (fun (k, v1) ->
      let ident = Option.value ~default:k (Environ.label k t1.locals) in
      let is_in = Variable.Set.mem (Variable.from_name k) in
      (ident, {
        control_dependent = is_in control_approx;
        data_dependent = is_in data_approx;
        state = (v1, Environ.get k t2.locals |> Option.value ~default:"?")
      })
    )
end

let box_tasks ~data_approx ~control_approx (t1:Task.t) (t2:Task.t) : PrintBox.t =
  let open PrintBox in
  let locals =
    (LocalState.parse_structs ~data_approx ~control_approx ~t1 ~t2
    @
    LocalState.parse_scalars ~data_approx ~control_approx ~t1 ~t2)
    |> List.sort (fun (i1, _) (i2, _) -> String.compare i1 i2)
    |> List.map (fun ((ident, ls):string * LocalState.t) ->
      let is_approx = ls.data_dependent || ls.control_dependent in
      let style = if is_approx then Style.bold else Style.default in
      let ident =
        if is_approx then
          let msg = if ls.control_dependent then "C" else "" in
          let msg = msg ^ (if ls.data_dependent then "D" else "") in
          ident ^ " (" ^ msg ^ ")"
        else
          ident
      in
      let (s1, s2) = ls.state in
      [| text_with_style style ident; text s1; text s2; |]
    )
  in
  let locals = Array.of_list locals in
  grid locals |> frame

let box_globals (w:Witness.t) : PrintBox.t =
  { w.globals with
  variables=
  [
    "index", Common.join " │ " w.indices;
  ]
  @ w.globals.variables
  }
  |> box_environ

let box_locals (w:Witness.t) : PrintBox.t =
  let (t1, t2) = w.tasks in
  box_tasks
    ~data_approx:w.data_approx
    ~control_approx:w.control_approx
    t1 t2

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
    le_index:int list;
    ge_index:int list;
    eq_index:int list;
    only_array:string option;
    thread_idx_1: Dim3.t option;
    thread_idx_2: Dim3.t option;
    arch: Architecture.t;
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
    ~ge_index
    ~le_index
    ~eq_index
    ~only_array
    ~thread_idx_1
    ~thread_idx_2
    ~arch
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
      kernels;
      ge_index;
      le_index;
      eq_index;
      only_array;
      thread_idx_1;
      thread_idx_2;
      arch;
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
      let p = Flatacc.translate a.arch p in
      show a.show_flat_acc (fun () -> Flatacc.print_kernels p);
      let p =
        p
        |> Symbexp.translate a.arch
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
  (block_dim:Dim3.t option)
  (grid_dim:Dim3.t option)
  (includes:string list)
  (ignore_calls:bool)
  (ge_index:int list)
  (le_index:int list)
  (eq_index:int list)
  (only_array:string option)
  (thread_idx_1:Dim3.t option)
  (thread_idx_2:Dim3.t option)
  (grid_level:bool)
:
  unit
=
  let arch = if grid_level then Architecture.Grid else Architecture.Block in
  let parsed = Protocol_parser.Silent.to_proto
    ~abort_on_parsing_failure:(not ignore_parsing_errors)
    ~includes
    ~block_dim
    ~grid_dim
    ~inline:(not ignore_calls)
    ~arch
    fname
  in
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
      ~ge_index
      ~le_index
      ~eq_index
      ~only_array
      ~thread_idx_1
      ~thread_idx_2
      ~arch
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

let block_dim =
  let d = Gv_parser.default_block_dim |> Dim3.to_string in
  let doc = "Sets the number of threads per block." ^ dim_help ^ "Default: " ^ d in
  Arg.(value & opt (some (conv_dim3 Dim3.one)) None & info ["b"; "block-dim"; "blockDim"] ~docv:"DIM3" ~doc)

let grid_dim =
  let d = Gv_parser.default_grid_dim |> Dim3.to_string in
  let doc = "Sets the number of blocks per grid." ^ dim_help ^ "Default: " ^ d in
  Arg.(value & opt (some (conv_dim3 Dim3.one)) None & info ["g"; "grid-dim"; "gridDim"] ~docv:"DIM3" ~doc)


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

let grid_level =
  let doc = "By default we perform block-level verification, this option performs grid-level verification." in
  Arg.(value & flag & info ["grid-level"] ~doc)


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
  $ grid_level
)

let info =
  let doc = "Verify if CUDA file is free from data races." in
  Cmd.info "faial-drf" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

