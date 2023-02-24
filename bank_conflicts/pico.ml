open Stage0
open Inference
open Bank_conflicts
open Protocols

module Solver = struct

  type t = {
    kernel: Proto.prog Proto.kernel;
    skip_zero: bool;
    use_maxima: bool;
    maxima_exe: string;
    use_absynth: bool;
    absynth_exe: string;
    use_cofloco: bool;
    cofloco_exe: string;
    use_koat: bool;
    koat_exe: string;
    show_code: bool;
    show_ra: bool;
    skip_simpl_ra: bool;
    asympt: bool;
    params: Params.t;
    count_shared_access: bool;
    explain: bool;
    show_ratio: bool;
  }

  let make
    ~kernel
    ~skip_zero
    ~use_maxima
    ~use_absynth
    ~use_cofloco
    ~use_koat
    ~absynth_exe
    ~cofloco_exe
    ~koat_exe
    ~show_code
    ~maxima_exe
    ~show_ra
    ~skip_simpl_ra
    ~distinct_vars
    ~asympt
    ~params
    ~count_shared_access
    ~explain
    ~show_ratio
  :
    t
  =
    let kernel =
      if distinct_vars then (
        let open Proto in
        let vars =
          Variable.Set.union
            kernel.kernel_global_variables
            kernel.kernel_local_variables
        in
        { kernel with
          kernel_code = Proto.vars_distinct kernel.kernel_code vars
        }
      ) else
        kernel
    in
    {
      kernel;
      skip_zero;
      use_maxima;
      use_absynth;
      use_cofloco;
      use_koat;
      absynth_exe;
      cofloco_exe;
      koat_exe;
      show_code;
      maxima_exe;
      show_ra;
      skip_simpl_ra;
      asympt;
      params;
      count_shared_access;
      explain;
      show_ratio;
    }

  let bank_conflict_count (app:t) : Exp.nexp -> int =
    Index_analysis.Default.analyze app.params app.kernel.kernel_local_variables

  let access_count (_:t) : Exp.nexp -> int =
    fun _ -> 1

  let access_analysis (app:t) : Exp.nexp -> int =
    if app.count_shared_access then
      access_count app
    else
      bank_conflict_count app

  type cost = {
    amount: string;
    analysis_duration: float;
  }

  let get_cost (app:t) (r:Ra.t) : (cost, string) Result.t =
    (if app.show_ra then (Ra.to_string r |> print_endline) else ());
    let start = Unix.gettimeofday () in
    (if app.use_absynth then
      r |> Absynth.run_ra ~verbose:app.show_code ~exe:app.absynth_exe ~asympt:app.asympt
    else if app.use_cofloco then
      r |> Cofloco.run_ra ~verbose:app.show_code ~exe:app.cofloco_exe ~asympt:app.asympt
    else if app.use_koat then
      r |> Koat.run_ra ~verbose:app.show_code ~exe:app.koat_exe ~asympt:app.asympt
    else if app.use_maxima then
      r |> Maxima.run_ra ~verbose:app.show_code ~exe:app.maxima_exe
    else
      r |> Symbolic.Default.run_ra ~show_code:app.show_code
    )
    |> Result.map (fun c ->
      {amount=c; analysis_duration=Unix.gettimeofday () -. start}
    )
    |> Result.map_error Errors.to_string

  let get_ra (a:t) (idx_analysis: Exp.nexp -> int) : Ra.t =
    let r = Ra.Default.from_kernel idx_analysis a.params a.kernel in
    if a.skip_simpl_ra then r else Ra.simplify r

  let total_cost (a:t) : (cost, string) Result.t =
    let r = get_ra a (access_analysis a) in
    get_cost a r

  let ratio_cost (a:t) : (cost, string) Result.t =
    let numerator = get_ra a (bank_conflict_count a) in
    let denominator = get_ra a (access_count a) in
    let start = Unix.gettimeofday () in
    Maxima.run_ra_ratio
      ~verbose:a.show_code
      ~exe:a.maxima_exe
      ~numerator
      ~denominator
    |> Result.map (fun c ->
      {amount=c; analysis_duration=Unix.gettimeofday () -. start}
    )
    |> Result.map_error Errors.to_string

  let sliced_cost (a:t) : (Shared_access.t * Ra.t * ((cost, string) Result.t)) Seq.t =
    let idx_analysis = access_analysis a in
    Shared_access.Default.from_kernel a.params a.kernel
    |> Seq.filter_map (fun s ->
      (* Convert a slice into an expression *)
      let r = Ra.Default.from_shared_access idx_analysis s in
      if Ra.is_zero r && a.skip_zero then
        None
      else
        Some (s, r, get_cost a r)
    )

  type summary =
    | TotalCost of (cost, string) Result.t
    | SlicedCost of (Shared_access.t * Ra.t * ((cost, string) Result.t)) Seq.t
    | RatioCost of (cost, string) Result.t

  let run (s:t) : summary =
    if s.explain then
      SlicedCost (sliced_cost s)
    else if s.show_ratio then
      RatioCost (ratio_cost s)
    else
      TotalCost (total_cost s)

end

module TUI = struct
  let run ~only_cost (s:Solver.t) =
    Stdlib.flush_all ();
    match Solver.run s with
    | RatioCost (Ok c)
    | TotalCost (Ok c) ->
      if only_cost then (
        print_endline c.amount
      ) else (
        let d = Float.to_int (c.analysis_duration *. 1000.) in
        print_string (s.kernel.kernel_name ^ " (" ^ string_of_int d  ^ "ms):\n");
        PrintBox.(
          c.amount
          |> text
          |> hpad 1
          |> frame
        )
        |> PrintBox_text.to_string
        |> print_endline
      )
    | RatioCost (Error e)
    | TotalCost (Error e) ->
      prerr_endline e;
      exit (-1)
    | SlicedCost s ->
      s
      |> Seq.iter (fun (s, r, c) ->
        let simplified_cost = match c with
          | Ok s -> Solver.(s.amount)
          | Error e ->
            Logger.Colors.error e;
            "???"
        in
        let blue = PrintBox.Style.(set_bold true (set_fg_color Blue default)) in
        let cost = match Symbolic.Default.from_ra r with
        | Ok e ->
            PrintBox.(tree ("▶ Cost: "  ^ Symbolic.to_string e |> text)
            [
              tree ("▶ Cost (simplified):" |> text_with_style blue)
              [
                text_with_style blue simplified_cost |> hpad 1
              ]
            ])
        | Error e ->
          Logger.Colors.error e;
          PrintBox.(tree ("▶ Cost (simplified):" |> text_with_style blue)
          [
            text_with_style blue simplified_cost |> hpad 1
          ])
        in
        (* Flatten the expression *)
        ANSITerminal.(print_string [Bold; Foreground Blue] ("\n~~~~ Bank-conflict ~~~~\n\n"));
        s |> Shared_access.location |> Tui.LocationUI.print;
        print_endline "";
        PrintBox.(
          tree (s |> Shared_access.to_string |> String.cat "▶ Context: " |> text)
          [cost]
        ) |> PrintBox_text.output stdout;
        print_endline "\n"
      )
end

module JUI = struct
  open Yojson.Basic
  type json = Yojson.Basic.t
  let to_json (s:Solver.t) : json =
    match Solver.run s with
    | TotalCost (Ok e) ->
      `Assoc [
        ("total_cost", `String e.amount);
        ("kernel_name", `String s.kernel.kernel_name);
        ("analysis_duration_seconds", `Float e.analysis_duration);
      ]
    | RatioCost (Ok e) ->
      `Assoc [
        ("ratio_cost", `String e.amount);
        ("kernel_name", `String s.kernel.kernel_name);
        ("analysis_duration_seconds", `Float e.analysis_duration);
      ]
    | RatioCost (Error e)
    | TotalCost (Error e) ->
      `Assoc [
        ("kernel_name", `String s.kernel.kernel_name);
        ("error", `String e);
      ]
    | SlicedCost s ->
      `List (s
      |> Seq.map (fun (s, _, e) ->
        let loc = Shared_access.location s in
        let loc = [
          "location",
            `Assoc [
              ("filename", `String loc.filename);
              ("line", `Int (Index.to_base1 loc.line));
              ("col_start", `Int (loc.interval |> Interval.start |> Index.to_base1));
              ("col_finish", `Int (loc.interval |> Interval.finish |> Index.to_base1));
            ]
          ]
        in
        let cost = match e with
          | Ok c ->
            let open Solver in
            [
            "cost", `String c.amount;
            "analysis_duration_seconds", `Float c.analysis_duration
            ]
          | Error e -> ["error", `String e]
        in
        `Assoc (loc @ cost)
      )
      |> List.of_seq
      )

  let run (s:Solver.t) : unit =
    s
    |> to_json
    |> to_string
    |> print_endline
end

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
  ~distinct_vars
  ~asympt
  ~only_cost
  ~params
  ~count_shared_access
  ~output_json
  ~show_ratio
  (k : Proto.prog Proto.kernel)
:
  unit
=
  let app : Solver.t = Solver.make
    ~use_maxima
    ~maxima_exe
    ~use_absynth
    ~absynth_exe
    ~use_cofloco
    ~cofloco_exe
    ~use_koat
    ~koat_exe
    ~show_code
    ~show_ra
    ~asympt
    ~skip_zero
    ~skip_simpl_ra
    ~distinct_vars
    ~params
    ~count_shared_access
    ~explain
    ~show_ratio
    ~kernel:k
  in
  if output_json then
    JUI.run app
  else
    TUI.run ~only_cost app


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
  (distinct_vars:bool)
  (only_cost:bool)
  (ignore_absent:bool)
  (asympt:bool)
  (count_shared_access:bool)
  (output_json:bool)
  (show_ratio: bool)
=
  let parsed = Protocol_parser.Silent.to_proto ~block_dim ~grid_dim fname in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  let params = Params.make ~block_dim ~grid_dim () in
  let proto =
    parsed.kernels
    |> List.filter Proto.has_shared_arrays
  in
  if ignore_absent || List.length proto > 0 then
    proto
    |> List.iter (fun k ->
      print_cost
        ~explain
        ~use_maxima
        ~use_absynth
        ~use_cofloco
        ~use_koat
        ~show_code
        ~show_ra
        ~skip_zero:(not show_all)
        ~distinct_vars
        ~absynth_exe
        ~maxima_exe
        ~cofloco_exe
        ~koat_exe
        ~skip_simpl_ra
        ~params
        ~only_cost
        ~asympt
        ~count_shared_access
        ~output_json
        ~show_ratio
        k
    )
  else (
    Logger.Colors.error "No kernels using __shared__ arrays found.";
    exit (-1)
  )

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

let distinct_vars =
  let doc = "Make all loop varibles distinct. This is a workaround for certain solvers." in
  Arg.(value & flag & info ["distinct-vars"] ~doc)

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

let show_ratio =
  let doc = "Show the ratio between bank-conflicts over number of shared " ^
  "accesses (ranges from 0 .. 31)."
  in
  Arg.(value & flag & info ["ratio"] ~doc)

let show_code =
  let doc = "Show the code being sent to the solver if any." in
  Arg.(value & flag & info ["show-code"] ~doc)

let count_shared_accesses =
  let doc = "Instead of counting how many bank-conflicts, count how many shared accesses occur." in
  Arg.(value & flag & info ["count-shared"] ~doc)

let output_json =
  let doc = "Output in JSON." in
  Arg.(value & flag & info ["json"] ~doc)

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
  $ distinct_vars
  $ only_cost
  $ ignore_absent
  $ asympt
  $ count_shared_accesses
  $ output_json
  $ show_ratio
)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-bc" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
