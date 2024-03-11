open Stage0
open Protocols
open Inference
open Drf

module Environ = Z3_solver.Environ
module Witness = Z3_solver.Witness
module StringMap = Common.StringMap
module T = ANSITerminal


(*
  Renders the local state as a PrintBox
  *)
module LocalState = struct
  type t = {
    ident: string;
    control_dependent: bool;
    data_dependent: bool;
    state: string * string;
  }

  let parse_structs (w:Witness.t) : t list =
    let t1_s = Environ.parse_structs (fst w.tasks).locals in
    let t2_s = Environ.parse_structs (snd w.tasks).locals in
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
      {
        ident;
        control_dependent = is_in w.control_approx;
        data_dependent = is_in w.data_approx;
        state=(to_string s1, to_string s2)
      }
    )

  let compare (x1:t) (x2:t) : int =
    String.compare x1.ident x2.ident

  let parse_scalars (w:Witness.t) : t list =
    let (t1, t2) = w.tasks in
    t1.locals
    |> Environ.remove_structs
    |> Environ.variables
    |> List.map (fun (k, v1) ->
      let ident = Option.value ~default:k (Environ.label k t1.locals) in
      let is_in = Variable.Set.mem (Variable.from_name k) in
      {
        ident;
        control_dependent = is_in w.control_approx;
        data_dependent = is_in w.data_approx;
        state = (v1, Environ.get k t2.locals |> Option.value ~default:"?")
      }
    )

  let from_witness (w:Witness.t) : t list =
    parse_structs w
    @
    parse_scalars w

  let render (ls : t) : PrintBox.t array =
    let open PrintBox in
    let is_approx = ls.data_dependent || ls.control_dependent in
    let style = if is_approx then Style.bold else Style.default in
    let ident =
      if is_approx then
        let msg = if ls.control_dependent then "C" else "" in
        let msg = msg ^ (if ls.data_dependent then "D" else "") in
        ls.ident ^ " (" ^ msg ^ ")"
      else
        ls.ident
    in
    let (s1, s2) = ls.state in
    [| text_with_style style ident; text s1; text s2; |]

  let to_print_box (data: t list) : PrintBox.t =
    data
    |> List.sort compare
    |> List.map render
    |> Array.of_list
    |> PrintBox.grid
    |> PrintBox.frame

end

module GlobalState = struct
  module Row = struct
    type t = {
      ident: string;
      control_dependent: bool;
      data_dependent: bool;
      state: string;
    }

    let parse_structs (w:Witness.t) : t list =
      w.globals
      |> Environ.parse_structs
      |> StringMap.bindings
      (* At this point we have the map of structs *)
      |> List.map (fun (ident, s) ->
        (* Calculuate the local variables, of a particular struct *)
        let all_vars : Variable.Set.t =
          s
          |> StringMap.bindings
          |> List.map (fun (field, _) ->
            ident ^ "." ^ field
            |> Variable.from_name
          )
          |> Variable.Set.of_list
        in
        let is_in (vs:Variable.Set.t) : bool =
          Variable.Set.cardinal (Variable.Set.inter vs all_vars) > 0
        in
        let state =
          s
          |> StringMap.bindings
          |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
          |> List.map (fun (f, v) ->
            f ^ " = " ^ v
          )
          |> String.concat " | "
        in
        {
          ident;
          control_dependent = is_in w.control_approx;
          data_dependent = is_in w.data_approx;
          state=state
        }
      )

    let parse_scalars (w:Witness.t) : t list
    =
      w.globals
      |> Environ.remove_structs
      |> Environ.variables
      |> List.map (fun (k, state) ->
        (* flag whether CI/DI *)
        let is_in = Variable.Set.mem (Variable.from_name k) in
        {
          (* get a nice label, rather than internal id if possible *)
          ident = Option.value ~default:k (Environ.label k w.globals);
          control_dependent = is_in w.control_approx;
          data_dependent = is_in w.data_approx;
          state;
        }
      )

    let from_witness (w:Witness.t) : t list =
      parse_structs w @ parse_scalars w

    let compare (x1:t) (x2:t) : int =
      String.compare x1.ident x2.ident

    let render (ls : t) : PrintBox.t array =
      let open PrintBox in
      let is_approx = ls.data_dependent || ls.control_dependent in
      let style = if is_approx then Style.bold else Style.default in
      let ident =
        if is_approx then
          let msg = if ls.control_dependent then "C" else "" in
          let msg = msg ^ (if ls.data_dependent then "D" else "") in
          ls.ident ^ " (" ^ msg ^ ")"
        else
          ls.ident
      in
      [| text_with_style style ident; text ls.state; |]
  end

  type t = {rows: Row.t list; index: string; state: string }

  let from_witness (w:Witness.t) : t =
    let brackets =
      List.map (fun _ -> "[]") w.indices
      |> Common.join ""
    in
    {
      index = w.array_name ^ brackets;
      state = Common.join " │ " w.indices;
      rows = Row.from_witness w;
    }

  let render_index (s:t) : PrintBox.t array =
    let open PrintBox in
    [| text_with_style Style.bold s.index; text s.state |]

  let to_print_box (s: t) : PrintBox.t =
    let rows =
      s.rows
      |> List.sort Row.compare
      |> List.map Row.render
    in
    render_index s :: rows
    |> Array.of_list
    |> PrintBox.grid
    |> PrintBox.frame

end

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
    block_idx_1: Dim3.t option;
    block_idx_2: Dim3.t option;
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
    ~block_idx_1
    ~block_idx_2
    ~arch
    (kernels: Proto.Code.t Proto.Kernel.t list)
  :
    t
  =
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
      block_idx_1;
      block_idx_2;
      arch;
    }

  let show (b:bool) (call:'a -> unit) (x:'a) : 'a =
    if b then call x else ();
    x

  let translate (a:t) (p:Proto.Code.t Proto.Kernel.t) : Flatacc.Kernel.t Streamutil.stream =
    p
    |> show a.show_proto (Proto.Kernel.print Proto.Code.to_s)
    (* 1. constant folding optimization *)
    |> Proto.Kernel.opt
    (* 2. convert to a well-formed protocol *)
    |> Wellformed.translate
    |> show a.show_wf Wellformed.print_kernels
    (* 3. align protocol *)
    |> Aligned.translate
    |> show a.show_align Aligned.print_kernels
    (* 3. split per sync *)
    |> Phasesplit.translate
    |> show a.show_phase_split Phasesplit.print_kernels
    (* 4. split per location *)
    |> Locsplit.translate
    |> Locsplit.filter_array a.only_array
    |> show a.show_loc_split Locsplit.print_kernels
    (* 5. flatten control-flow structures *)
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
            ~show_proofs:a.show_proofs
            ~logic:a.logic
        |> Streamutil.to_list
      in
      Stdlib.flush_all ();
      Analysis.{kernel; report}
    )


end

let tui (output: Analysis.t list) : unit =
  let total = ref 0 in
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
        w |> GlobalState.from_witness |> GlobalState.to_print_box |> print_box;
        T.print_string [T.Bold] ("\n\nLocals\n");
        w |> LocalState.from_witness |> LocalState.to_print_box |> print_box;
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
      if err_count <> "0" || has_unknown then (
        total := !total + 1;
      ) else
        ()
  )
  ;
  if !total > 0 then
    exit 1
  else
    ()

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

let set_block_idx
  (bid: Dim3.t)
  (ks: Proto.Code.t Proto.Kernel.t list)
:
  Proto.Code.t Proto.Kernel.t list
=
  let kvs = Dim3.to_assoc ~prefix:"blockIdx." bid in
  List.map (Proto.Kernel.assign_globals kvs) ks

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
  (block_idx_1:Dim3.t option)
  (block_idx_2:Dim3.t option)
  (grid_level:bool)
  (unreachable:bool)
:
  unit
=
  let arch = if grid_level then Architecture.Grid else Architecture.Block in
  if Option.is_some block_idx_2 && not (Architecture.is_grid arch) then (
    prerr_endline ("ERROR: Can only use --block-idx-2 with --grid-level.");
    exit 1
  ) else
  let parsed = Protocol_parser.Silent.to_proto
    ~abort_on_parsing_failure:(not ignore_parsing_errors)
    ~includes
    ~block_dim
    ~grid_dim
    ~inline:(not ignore_calls)
    ~arch
    fname
  in
  let kernels = parsed.kernels in
  (* Assign bid *)
  let kernels =
    match block_idx_1, arch with
    | Some block_idx_1, Architecture.Block ->
      set_block_idx block_idx_1 kernels
    | _, _ -> kernels
  in
  let ui = if output_json then jui else tui in
  kernels
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
      ~block_idx_1
      ~block_idx_2
      ~arch
  |> (fun a ->
    if unreachable then
      App.check_unreachable a
    else
      App.run a |> ui
  )


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

let unreachable =
  let doc = "Check unreachable accesses." in
  Arg.(value & flag & info ["unreachable"] ~doc)

let ignore_parsing_errors =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info ["ignore-parsing-errors"] ~doc)

let dim_help = {|
The value will be loaded from header if omitted.
Examples (without quotes): "[2,2,2]" or "32".
|} |> Common.replace ~substring:"\n" ~by:""

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
  let doc = "Sets the number of threads per block." ^ dim_help ^ " Default: " ^ d in
  Arg.(value & opt (some (conv_dim3 Dim3.one)) None & info ["b"; "block-dim"; "blockDim"] ~docv:"DIM3" ~doc)

let grid_dim =
  let d = Gv_parser.default_grid_dim |> Dim3.to_string in
  let doc = "Sets the number of blocks per grid." ^ dim_help ^ " Default: " ^ d in
  Arg.(value & opt (some (conv_dim3 Dim3.one)) None & info ["g"; "grid-dim"; "gridDim"] ~docv:"DIM3" ~doc)


let thread_idx_1 =
  let doc = "Sets the thread index for one thread." ^ dim_help ^ " Default: (none)" in
  Arg.(value & opt (some (conv_dim3 Dim3.zero)) None & info ["thread-idx-1"; "tid1"] ~docv:"DIM3" ~doc)

let thread_idx_2 =
  let doc = "Sets the thread index for another thread." ^ dim_help ^ " Default: (none)" in
  Arg.(value & opt (some (conv_dim3 Dim3.zero)) None & info ["thread-idx-2"; "tid2"] ~docv:"DIM3" ~doc)

let block_idx_1 =
  let doc = "Sets the block index for one thread. Only available in grid-level analysis." ^ dim_help ^ " Default: (none)" in
  Arg.(value & opt (some (conv_dim3 Dim3.zero)) None & info ["block-idx-1"; "bid1"] ~docv:"DIM3" ~doc)

let block_idx_2 =
  let doc = "Sets the block index for another thread. Only available in grid-level analysis." ^ dim_help ^ " Default: (none)" in
  Arg.(value & opt (some (conv_dim3 Dim3.zero)) None & info ["block-idx-2"; "bid2"] ~docv:"DIM3" ~doc)

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
  let print f (l:int list) : unit =
    let s = "[" ^ (List.map string_of_int l |> String.concat ", ") ^ "]" in
    Format.fprintf f "%s" s
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
  $ block_idx_1
  $ block_idx_2
  $ grid_level
  $ unreachable
)

let info =
  let doc = "Verify if CUDA file is free from data races." in
  Cmd.info "faial-drf" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

