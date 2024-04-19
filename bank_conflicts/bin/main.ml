open Stage0
open Inference
open Bank_conflicts
open Protocols

type kernel = Proto.Code.t Proto.Kernel.t

let abort_when (b:bool) (msg:string) : unit =
  if b then (
    Logger.Colors.error msg;
    exit (-2)
  ) else ()


module Solver = struct

  type t = {
    kernels: kernel list;
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
    config: Config.t;
    count_shared_access: bool;
    explain: bool;
    per_request: bool;
    ignore_absent: bool;
    only_reads: bool;
    only_writes: bool;
    block_dim: Dim3.t;
    grid_dim: Dim3.t;
    params: (string * int) list;
  }

  let make
    ~kernels
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
    ~skip_distinct_vars
    ~asympt
    ~config
    ~count_shared_access
    ~explain
    ~per_request
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
  :
    t
  =
    let kernels =
      if skip_distinct_vars then
        kernels
      else
        List.map Proto.Kernel.vars_distinct kernels
    in
    {
      kernels;
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
      config;
      count_shared_access;
      explain;
      per_request;
      ignore_absent;
      only_reads;
      only_writes;
      block_dim;
      grid_dim;
      params;
    }

  let bank_conflict_count (app:t) : Variable.Set.t -> Exp.nexp -> int =
    fun locals idx ->
      (Index_analysis.Default.transaction_count app.config locals idx) - 1

  let transaction_count (app:t) : Variable.Set.t -> Exp.nexp -> int =
      Index_analysis.Default.transaction_count app.config

  let access_count (_:t) : Variable.Set.t -> Exp.nexp -> int =
    fun _ _ -> 1

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
    else if app.use_maxima then (
      abort_when app.asympt "The Maxima backend does not support asympotic cost.";
      r |> Maxima.run_ra ~verbose:app.show_code ~exe:app.maxima_exe
    ) else
      r |> Symbolic.Default.run_ra ~show_code:app.show_code
    )
    |> Result.map (fun c ->
      {amount=c; analysis_duration=Unix.gettimeofday () -. start}
    )
    |> Result.map_error Errors.to_string

  let get_ra (a:t) (k:kernel) (idx_analysis: Variable.Set.t -> Exp.nexp -> int) : Ra.t =
    let r = Ra.Default.from_kernel idx_analysis a.config k in
    if a.skip_simpl_ra then r else Ra.simplify r

  type r_cost = (cost, string) Result.t
  type slice = Access_context.t * Ra.t * r_cost

  let total_cost (a:t) (k:kernel) : r_cost =
    let metric =
      if a.count_shared_access then
        access_count a
      else
        bank_conflict_count a
    in
    let r = get_ra a k metric in
    get_cost a r

  let transactions_per_request (a:t) (k:kernel) : r_cost =
    let numerator = get_ra a k (transaction_count a) in
    let denominator = get_ra a k (access_count a) in
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

  let sliced_cost (a:t) (k:kernel) : slice list =
    let idx_analysis =
      if a.count_shared_access then
        access_count a
      else if a.per_request then
        transaction_count a
      else
        bank_conflict_count a
    in
    Access_context.Default.from_kernel a.config k
    |> Seq.filter_map (fun s ->
      (* Convert a slice into an expression *)
      let r =
        Ra.Default.from_access_context
          idx_analysis
          (Params.to_set k.local_variables)
          s
      in
      if Ra.is_zero r && a.skip_zero then
        None
      else
        Some (s, r, get_cost a r)
    )
    |> List.of_seq

  type summary =
    | TotalCost of (kernel * r_cost) list
    | SlicedCost of (kernel * slice list) list
    | RatioCost of (kernel * r_cost) list

  let run (s:t) : summary =
    let pair f k =
      (k, f k)
    in
    (* optimize *)
    let retain_acc =
      if s.only_reads then
        Protocols.Access.is_read
      else if s.only_writes then
        Protocols.Access.is_write
      else
        fun a ->
          Protocols.Access.is_read a || Protocols.Access.is_write a
    in
    let ks =
      s.kernels
      |> List.map (Proto.Kernel.filter_access retain_acc)
      |> List.map (
          Proto.Kernel.inline_all
          ~block_dim:s.block_dim
          ~grid_dim:s.grid_dim
          ~globals:s.params
        )
      |> List.map Proto.Kernel.opt
    in
    if s.explain then
      SlicedCost (List.map (pair (sliced_cost s)) ks)
    else if s.per_request then
      RatioCost (List.map (pair (transactions_per_request s)) ks)
    else
      TotalCost (List.map (pair (total_cost s)) ks)

end

module TUI = struct

  let run ~only_cost (s:Solver.t) =
    let print_r_cost ((k:kernel), (r:Solver.r_cost)) : unit =
      match r with
      | Ok c ->
        if only_cost then (
          print_endline c.amount
        ) else (
          let d = Float.to_int (c.analysis_duration *. 1000.) in
          print_string (k.name ^ " (" ^ string_of_int d  ^ "ms):\n");
          PrintBox.(
            c.amount
            |> text
            |> hpad 1
            |> frame
          )
          |> PrintBox_text.to_string
          |> print_endline
        )
      | Error e ->
        prerr_endline e;
        exit (-1)
    in
    let print_slice ((k:kernel), (s:Solver.slice list)) : unit =
      ANSITerminal.(print_string [Bold; Foreground Green] ("\n### Kernel '" ^ k.name ^ "' ###\n\n"));
      Logger.Colors.info ("Shared accesses found: " ^ string_of_int (List.length s));
      Stdlib.flush_all ();
      s
      |> List.iter (fun (s, r, c) ->
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
        s |> Access_context.location |> Tui_helper.LocationUI.print;
        print_endline "";
        PrintBox.(
          tree (s |> Access_context.to_string |> String.cat "▶ Context: " |> text)
          [cost]
        ) |> PrintBox_text.output stdout;
        print_endline "\n"
      )
    in
    Stdlib.flush_all ();
    match Solver.run s with
    | RatioCost []
    | TotalCost []
    | SlicedCost [] ->
      abort_when (not s.ignore_absent) "No kernels using __shared__ arrays found.";
    | TotalCost l
    | RatioCost l ->
      List.iter print_r_cost l
    | SlicedCost l ->
      List.iter print_slice l
end

module JUI = struct
  open Yojson.Basic
  type json = Yojson.Basic.t
  let to_json (s:Solver.t) : json =
    let c_to_j name ((k:kernel), r) : json =
      let open Solver in
      match r with
      | Ok e ->
        `Assoc [
          (name, `String e.amount);
          ("kernel_name", `String k.name);
          ("analysis_duration_seconds", `Float e.analysis_duration);
        ]
      | Error e ->
        `Assoc [
          ("kernel_name", `String k.name);
          ("error", `String e);
        ]
    in
    let s_to_j ((k:kernel), l) : json =
      let accs : json =
        `List (
          l
          |> List.map (fun (s, _, e) ->
            let loc = Access_context.location s in
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
        )
      in
      `Assoc [
        "kernel_name", `String k.name;
        "accesses", accs;
      ]
    in
    let kernels =
      match Solver.run s with
      | TotalCost l -> `List (List.map (c_to_j "total_cost") l)
      | RatioCost l -> `List (List.map (c_to_j "per_transaction") l)
      | SlicedCost s -> `List (List.map s_to_j s)
    in
    `Assoc [
      "kernels", kernels;
      "argv", `List (Sys.argv |> Array.to_list |> List.map (fun x -> `String x));
      "executable_name", `String Sys.executable_name;
      "z3_version", `String (Z3.Version.to_string);
    ]

  let run (s:Solver.t) : unit =
    s
    |> to_json
    |> to_string
    |> print_endline
end

let run
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
  ~skip_distinct_vars
  ~asympt
  ~only_cost
  ~config
  ~count_shared_access
  ~output_json
  ~ignore_absent
  ~per_request
  ~only_reads
  ~only_writes
  ~block_dim
  ~grid_dim
  ~params
  (kernels : Proto.Code.t Proto.Kernel.t list)
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
    ~skip_distinct_vars
    ~config
    ~count_shared_access
    ~explain
    ~per_request
    ~kernels
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
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
  (skip_distinct_vars:bool)
  (only_cost:bool)
  (ignore_absent:bool)
  (asympt:bool)
  (count_shared_access:bool)
  (output_json:bool)
  (per_request:bool)
  (only_reads:bool)
  (only_writes:bool)
  (params:(string * int) list)
=
  let parsed = Protocol_parser.Silent.to_proto ~block_dim ~grid_dim fname in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  let config = Config.make ~block_dim ~grid_dim () in
  let kernels : kernel list =
    parsed.kernels
    |> List.filter Proto.Kernel.has_shared_arrays
  in
  run
    ~explain
    ~use_maxima
    ~use_absynth
    ~use_cofloco
    ~use_koat
    ~show_code
    ~show_ra
    ~skip_zero:(not show_all)
    ~skip_distinct_vars
    ~absynth_exe
    ~maxima_exe
    ~cofloco_exe
    ~koat_exe
    ~skip_simpl_ra
    ~config
    ~only_cost
    ~asympt
    ~count_shared_access
    ~output_json
    ~per_request
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    kernels


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

let skip_distinct_vars =
  let doc = "By default we make all loop varibles distinct, as a workaround for certain solvers' limitations." in
  Arg.(value & flag & info ["skip-distinct-vars"] ~doc)

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

let per_request =
  let doc = "Average number of shared memorytransactions performed for each " ^
  "shared memory access."
  in
  Arg.(value & flag & info ["per-request"] ~doc)

let show_code =
  let doc = "Show the code being sent to the solver if any." in
  Arg.(value & flag & info ["show-code"] ~doc)

let count_shared_accesses =
  let doc = "Instead of counting how many bank-conflicts, count how many shared accesses occur." in
  Arg.(value & flag & info ["count-shared"] ~doc)

let output_json =
  let doc = "Output in JSON." in
  Arg.(value & flag & info ["json"] ~doc)

let only_reads =
  let doc = "Only account for load transactions (access reads)." in
  Arg.(value & flag & info ["only-reads"] ~doc)

let only_writes =
  let doc = "Only account for store transactions (access writes)." in
  Arg.(value & flag & info ["only-writes"] ~doc)

let params =
  let doc = "Set the value of an integer parameter" in
  Arg.(value & opt_all (pair ~sep:'=' string int) [] & info ["p"; "param"] ~docv:"KEYVAL" ~doc)

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
  $ skip_distinct_vars
  $ only_cost
  $ ignore_absent
  $ asympt
  $ count_shared_accesses
  $ output_json
  $ per_request
  $ only_reads
  $ only_writes
  $ params
)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-bc" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
