open Stage0
open Inference
open Bank_conflicts
open Protocols

(* Main function *)

let print_cost
  ?(skip_zero=true)
  ?(use_maxima=false)
  ?(use_absynth=false)
  ?(use_cofloco=false)
  ?(use_koat=false)
  ?(explain=true)
  ?(absynth_exe="absynth")
  ?(cofloco_exe="cofloco")
  ?(koat_exe="koat2")
  ?(show_code=false)
  ?(maxima_exe="maxima")
  ?(show_ra=false)
  ?(skip_simpl_ra=true)
  ~asympt
  ~only_cost
  ~params
  (k : Proto.prog Proto.kernel)
:
  unit
=
  let with_ra (k:Proto.prog Proto.kernel) : unit =
    let r = Ra.Default.from_kernel params k in
    let r = if skip_simpl_ra then r else Ra.simplify r in
    (if show_ra then (Ra.to_string r |> print_endline) else ());
    match
      if use_absynth then
        r |> Absynth.run_ra ~verbose:show_code ~exe:absynth_exe ~asympt
      else if use_cofloco then
        r |> Cofloco.run_ra ~verbose:show_code ~exe:cofloco_exe ~asympt
      else if use_koat then
        r |> Koat.run_ra ~verbose:show_code ~exe:koat_exe ~asympt
      else if use_maxima then
        r |> Maxima.run_ra ~verbose:show_code ~exe:maxima_exe
      else (
        (if show_code then (Ra.to_string r |> print_endline) else ());
        Ok (Symbolic.Default.from_ra r |> Symbolic.Default.simplify)
      )
    with
    | Ok cost ->
      Stdlib.flush_all ();
      if only_cost then (
        print_endline cost
      ) else (
        print_string (k.kernel_name ^ ":\n");
        PrintBox.(
          cost
          |> text
          |> hpad 1
          |> frame
        )
        |> PrintBox_text.to_string
        |> print_endline
      )
    | Error e ->
      prerr_endline (Errors.to_string e);
      exit (-1)
  in
  let with_slices (k:Proto.prog Proto.kernel) : unit =
    let render_s ?(show_code=false) (s: Symbolic.t) : (string, Errors.t) Result.t =
      if use_maxima then
        Maxima.run_symbolic ~verbose:show_code ~exe:maxima_exe s
      else if use_absynth then
        Absynth.run_symbolic ~verbose:show_code ~exe:absynth_exe s
      else if use_cofloco then
        Cofloco.run_symbolic ~verbose:show_code ~exe:cofloco_exe s
      else if use_koat then
        Koat.run_symbolic ~verbose:show_code ~exe:koat_exe s
      else
        Ok (Symbolic.Default.simplify s)
    in
    Shared_access.Default.from_kernel params k
    |> Seq.iter (fun s ->
      (* Convert a slice into an expression *)
      let s1 = Symbolic.Default.from_slice params k.kernel_local_variables s in
      if skip_zero && Symbolic.is_zero s1 then
        ()
      else
        (* Flatten the expression *)
        let simplified_cost = match render_s ~show_code s1 with
          | Ok s -> s
          | Error e ->
            prerr_endline (Errors.to_string e);
            "???"
        in
        ANSITerminal.(print_string [Bold; Foreground Blue] ("\n~~~~ Bank-conflict ~~~~\n\n"));
        s |> Shared_access.location |> Tui.LocationUI.print;
        print_endline "";
        let blue = PrintBox.Style.(set_bold true (set_fg_color Blue default)) in
        PrintBox.(
          tree (s |> Shared_access.to_string |> String.cat "▶ Context: " |> text)
          [
            tree ("▶ Cost: "  ^ Symbolic.to_string s1 |> text)
            [
              tree ("▶ Cost (simplified):" |> text_with_style blue)
              [
                text_with_style blue simplified_cost |> hpad 1
              ]
            ]
          ]
        ) |> PrintBox_text.output stdout;
        print_endline "\n";
    );
  in
  (* 1. break a kernel into slices *)
  if explain then (
    with_slices k
  ) else (
    with_ra k
  )


let pico
  (fname : string)
  (block_dim:Dim3.t option)
  (grid_dim:Dim3.t option)
  (use_maxima:bool)
  (use_absynth:bool)
  (use_cofloco:bool)
  (use_koat:bool)
  (show_all:bool)
  (show_ra:bool)
  (explain:bool)
  (show_code:bool)
  (absynth_exe:string)
  (cofloco_exe:string)
  (koat_exe:string)
  (maxima_exe:string)
  (skip_simpl_ra:bool)
  (only_cost:bool)
  (ignore_absent:bool)
  (asympt:bool)
=
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let gv = match Gv_parser.parse fname with
      | Some gv ->
        Logger.Colors.info ("Found GPUVerify args in source file: " ^ Gv_parser.to_string gv);
        gv
      | None -> Gv_parser.default
    in
    let kvs = Gv_parser.to_assoc gv in
    let block_dim = block_dim |> Option.value ~default:gv.block_dim in
    let grid_dim = grid_dim |> Option.value ~default:gv.grid_dim in
    let params = Params.make ~block_dim ~grid_dim () in
    let c_ast = parsed_json |> C_lang.parse_program |> Result.get_ok in
    let d_ast = c_ast |> D_lang.rewrite_program in
    let imp = d_ast |> D_to_imp.Silent.parse_program |> Result.get_ok in
    let proto =
      imp
      |> List.map Imp.compile
      |> List.map (Proto.replace_constants kvs)
      |> List.filter Proto.has_shared_arrays
    in
    if ignore_absent || List.length proto > 0 then
      List.iter (fun k ->
        print_cost
          ~explain
          ~use_maxima
          ~use_absynth
          ~use_cofloco
          ~use_koat
          ~show_code
          ~show_ra
          ~skip_zero:(not show_all)
          ~absynth_exe
          ~maxima_exe
          ~cofloco_exe
          ~koat_exe
          ~skip_simpl_ra
          ~params
          ~only_cost
          ~asympt
          k
      ) proto
    else (
      Logger.Colors.error "No kernels using __shared__ arrays found.";
      exit (-1)
    )
  with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)

(* Command-line interface *)

open Cmdliner


let dim3 : Dim3.t Cmdliner.Arg.conv =
  let parse =
    fun s ->
      match Dim3.parse s with
      | Ok r -> Ok r
      | Error e -> Error (`Msg e)
  in
  let print : Dim3.t Cmdliner.Arg.printer =
    fun ppf v -> Format.fprintf ppf "%s" (Dim3.to_string v)
  in
  Arg.conv (parse, print)

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let block_dim =
  let doc = "Sets the CUDA variable blockDim, the number of threads per block.\n" ^
  "Input is a single integer or a list of integers, signifying the x, y, and z positions.\n" ^
  "Default: '" ^ Dim3.to_string Gv_parser.default_block_dim ^ "'\n" ^
  "Examples: '1024' and '[16,16]'" in
  Arg.(value & opt (some dim3) None & info ["b"; "block-dim"; "blockDim"] ~docv:"BLOCK_DIM" ~doc)

let grid_dim =
  let doc = "Sets the CUDA variable gridDim, the number of blocks per grid.\n" ^
  "Input is a single integer or a list of integers, signifying the x, y, and z positions.\n" ^
  "Default: '" ^ Dim3.to_string Gv_parser.default_grid_dim ^ "'\n" ^
  " Examples: '1024' and '[16,16]'" in
  Arg.(value & opt (some dim3) None & info ["g"; "grid-dim"; "gridDim"] ~docv:"GRID_DIM" ~doc)

let absynth_exe =
  let doc = "Sets the path to the absynth executable." in
  Arg.(value & opt string "absynth" & info ["absynth-exe"] ~doc)

let cofloco_exe =
  let doc = "Sets the path to the CoFloCo executable." in
  Arg.(value & opt string "cofloco" & info ["cofloco-exe"] ~doc)

let koat_exe =
  let doc = "Sets the path to the KoAT2 executable." in
  Arg.(value & opt string "koat2" & info ["koat-exe"] ~doc)

let maxima_exe =
  let doc = "Sets the path to the Maxima executable." in
  Arg.(value & opt string "maxima" & info ["maxima-exe"] ~doc)

let use_maxima =
  let doc = "Uses maxima to simplify the cost of each access." in
  Arg.(value & flag & info ["maxima"] ~doc)

let use_absynth =
  let doc = "Uses absynth to simplify the cost of each access." in
  Arg.(value & flag & info ["absynth"] ~doc)

let use_cofloco =
  let doc = "Uses CoFloCo to simplify the cost of each access." in
  Arg.(value & flag & info ["cofloco"] ~doc)

let only_cost =
  let doc = "Only prints out the cost, no kernel name, and no UI flourishes. This option is only available when computing the total cost (default option)." in
  Arg.(value & flag & info ["only-cost"] ~doc)

let use_koat =
  let doc = "Uses KoAT2 to simplify the cost of each access." in
  Arg.(value & flag & info ["koat"] ~doc)

let ignore_absent =
  let doc = "Makes it not an error to analyze a kernel without shared errors." in
  Arg.(value & flag & info ["ignore-absent"] ~doc)

let skip_simpl_ra =
  let doc = "By default we simplify the RA to improve performance of solvers." in
  Arg.(value & flag & info ["skip-simpl-ra"] ~doc)

let show_all =
  let doc = "By default we skip accesses that yield 0 bank-conflicts." in
  Arg.(value & flag & info ["show-all"] ~doc)

let show_ra =
  let doc = "Print out the resource-analysis problem that represents the bank conflicts." in
  Arg.(value & flag & info ["show-ra"] ~doc)

let asympt =
  let doc = "Calculate the asymptotic cost of bank conflicts." in
  Arg.(value & flag & info ["asympt"] ~doc)

let explain =
  let doc = "Show bank-conflicts per location." in
  Arg.(value & flag & info ["explain"] ~doc)

let show_code =
  let doc = "Show the code being sent to the solver if any." in
  Arg.(value & flag & info ["show-code"] ~doc)

let pico_t = Term.(
  const pico
  $ get_fname
  $ block_dim
  $ grid_dim
  $ use_maxima
  $ use_absynth
  $ use_cofloco
  $ use_koat
  $ show_all
  $ show_ra
  $ explain
  $ show_code
  $ absynth_exe
  $ cofloco_exe
  $ koat_exe
  $ maxima_exe
  $ skip_simpl_ra
  $ only_cost
  $ ignore_absent
  $ asympt
)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-bc" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
