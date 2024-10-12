open Stage0
open Inference
open Bank_conflicts
open Protocols
open Ra
open Analyze_cost

type kernel = Protocols.Kernel.t

let abort_when (b:bool) (msg:string) : unit =
  if b then (
    Logger.Colors.error msg;
    exit (-2)
  ) else ()

module Solver = struct

  type t = {
    kernels: kernel list;
    use_maxima: bool;
    maxima_exe: string;
    use_absynth: bool;
    absynth_exe: string;
    use_cofloco: bool;
    cofloco_exe: string;
    use_koat: bool;
    koat_exe: string;
    show_code: bool;
    show_map: bool;
    show_ra: bool;
    skip_simpl_ra: bool;
    asympt: bool;
    config: Config.t;
    ignore_absent: bool;
    only_reads: bool;
    only_writes: bool;
    block_dim: Dim3.t;
    grid_dim: Dim3.t;
    params: (string * int) list;
    approx_ifs: bool;
    strategy: Summation.Strategy.t;
    metric: Metric.t;
    compact: bool;
  }

  let make
    ~kernels
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
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    ~approx_ifs
    ~strategy
    ~metric
    ~compact
    ~show_map
  :
    t
  =
    let kernels =
      if skip_distinct_vars then
        kernels
      else
        List.map Protocols.Kernel.vars_distinct kernels
    in
    {
      kernels;
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
      ignore_absent;
      only_reads;
      only_writes;
      block_dim;
      grid_dim;
      params;
      approx_ifs;
      strategy;
      metric;
      compact;
      show_map;
    }

  let gen_cost
    (app:t)
    (default:int)
    (s:Variable.Set.t)
    (e:Exp.nexp)
  :
    int
  =
    let e = Offset_analysis.Default.remove_offset s e in
    let ctx = Vectorized.from_config app.config in
    match Vectorized.to_cost app.metric e ctx with
    | Ok e -> e.value
    | Error e ->
      Logger.Colors.warning e;
      default

  let min_cost (app:t) : Variable.Set.t -> Exp.nexp -> int =
    gen_cost app (Metric.min_cost app.metric |> Cost.value)

  let max_cost (app:t) : Variable.Set.t -> Exp.nexp -> int =
    gen_cost app (Metric.max_cost app.config app.metric |> Cost.value)

  type cost = {
    amount: string;
    analysis_duration: float;
  }

  let get_cost (app:t) (r:Ra.Stmt.t) : (cost, string) Result.t =
    (if app.show_ra then (Ra.Stmt.to_string r |> print_endline) else ());
    let start = Unix.gettimeofday () in
    (if app.use_absynth then
      r |> Absynth.run ~verbose:app.show_code ~exe:app.absynth_exe ~asympt:app.asympt
    else if app.use_cofloco then
      r |> Cofloco.run ~verbose:app.show_code ~exe:app.cofloco_exe ~asympt:app.asympt
    else if app.use_koat then
      r |> Koat.run ~verbose:app.show_code ~exe:app.koat_exe ~asympt:app.asympt
    else if app.use_maxima then (
      abort_when app.asympt "The Maxima backend does not support asympotic cost.";
      r |> Maxima.run ~verbose:app.show_code ~exe:app.maxima_exe ~compact:app.compact
    ) else
      Ok (r |> Summation.run ~show_code:app.show_code)
    )
    |> Result.map (fun c ->
      {amount=c; analysis_duration=Unix.gettimeofday () -. start}
    )
    |> Result.map_error Errors.to_string

  let get_ra
    (a:t)
    (k:kernel)
    (idx_analysis: Variable.Set.t -> Exp.nexp -> int)
  :
    Ra.Stmt.t
  =
    let strategy =
      if a.approx_ifs then
        Ra_compiler.Approximate
      else
        Ra_compiler.Exact
    in
    let r = Ra_compiler.Default.from_kernel ~strategy idx_analysis a.config k in
    if a.skip_simpl_ra then r else Ra.Stmt.simplify r

  type r_cost = (cost, string) Result.t

  let total_cost (a:t) (k:kernel) : r_cost =
    let s =
      if a.strategy = Summation.Strategy.Max then
        max_cost a
      else min_cost a
    in
    let r = get_ra a k s in
    get_cost a r

  type summary =
    | TotalCost of (kernel * r_cost) list

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
      |> (
        if s.only_reads || s.only_writes then
          List.map (Protocols.Kernel.filter_access retain_acc)
        else
          fun x -> x
        )
      |> List.map (fun k ->
          if s.metric = CountAccesses then k else
          let vs : Variable.Set.t =
            match s.metric with
            | BankConflicts -> Protocols.Kernel.shared_arrays k
            | UncoalescedAccesses -> Protocols.Kernel.global_arrays k
            | _ -> failwith "internal error"
          in
          Protocols.Kernel.filter_array (fun x -> Variable.Set.mem x vs) k
        )
      |> List.map (
          Protocols.Kernel.inline_all
          ~block_dim:(Some s.block_dim)
          ~grid_dim:(Some s.grid_dim)
          ~globals:s.params
        )
      |> List.map Protocols.Kernel.opt
      |> List.map (fun p ->
        if s.show_map then
          Protocols.Kernel.print p
        ;
        p)
    in
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
    Stdlib.flush_all ();
    match Solver.run s with
    | TotalCost [] ->
      abort_when (not s.ignore_absent) "No kernels found.";
    | TotalCost l ->
      List.iter print_r_cost l
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
    let kernels =
      match Solver.run s with
      | TotalCost l -> `List (List.map (c_to_j "total_cost") l)
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
  ?(use_maxima=false)
  ?(use_absynth=false)
  ?(use_cofloco=false)
  ?(use_koat=false)
  ?(absynth_exe="absynth")
  ?(cofloco_exe="cofloco")
  ?(koat_exe="koat2")
  ?(show_code=false)
  ?(maxima_exe="maxima")
  ?(show_ra=false)
  ?(show_map=false)
  ?(skip_simpl_ra=true)
  ~skip_distinct_vars
  ~asympt
  ~only_cost
  ~config
  ~output_json
  ~ignore_absent
  ~only_reads
  ~only_writes
  ~block_dim
  ~grid_dim
  ~params
  ~approx_ifs
  ~strategy
  ~metric
  ~compact
  (kernels : Protocols.Kernel.t list)
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
    ~skip_simpl_ra
    ~skip_distinct_vars
    ~config
    ~kernels
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    ~approx_ifs
    ~strategy
    ~metric
    ~compact
    ~show_map
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
  (show_ra:bool)
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
  (output_json:bool)
  (only_reads:bool)
  (only_writes:bool)
  (params:(string * int) list)
  (approx_ifs:bool)
  (strategy:Summation.Strategy.t)
  (metric:Metric.t)
  (compact:bool)
  (ignore_parsing_errors:bool)
  (show_map:bool)
=
  let parsed = Protocol_parser.Silent.to_proto
    ~abort_on_parsing_failure:(not ignore_parsing_errors)
    ~block_dim
    ~grid_dim
    fname in
  let block_dim = parsed.options.block_dim in
  let grid_dim = parsed.options.grid_dim in
  let config = Config.make ~block_dim ~grid_dim () in
  run
    ~use_maxima
    ~use_absynth
    ~use_cofloco
    ~use_koat
    ~show_code
    ~show_ra
    ~skip_distinct_vars
    ~absynth_exe
    ~maxima_exe
    ~cofloco_exe
    ~koat_exe
    ~skip_simpl_ra
    ~config
    ~only_cost
    ~asympt
    ~output_json
    ~ignore_absent
    ~only_reads
    ~only_writes
    ~block_dim
    ~grid_dim
    ~params
    ~approx_ifs
    ~strategy
    ~metric
    ~compact
    ~show_map
    parsed.kernels


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

let show_ra =
  let doc = "Print out the resource-analysis problem that represents the bank conflicts." in
  Arg.(value & flag & info ["show-ra"] ~doc)

let asympt =
  let doc = "Calculate the asymptotic cost of bank conflicts." in
  Arg.(value & flag & info ["asympt"] ~doc)

let show_code =
  let doc = "Show the code being sent to the solver if any." in
  Arg.(value & flag & info ["show-code"] ~doc)

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

let approx_ifs =
  let doc = "Approximate conditionals." in
  Arg.(value & flag & info ["approx"] ~doc)

let strategy =
  let doc = "Generate minimum cost for approximate costs (default is maximum cost)." in
  Arg.(value & opt (enum ["min", Summation.Strategy.Min; "max", Summation.Strategy.Max]) Summation.Strategy.Max & info ["cost"] ~doc)

let metric =
  let doc = "Select the metric to measure the cost." in
  Arg.(required & opt (some (enum Metric.choices)) None & info ["m"; "metric"] ~doc)

let compact =
  let doc = "Render Maxima formulas on a single line." in
  Arg.(value & flag & info ["compact"] ~doc)

let ignore_parsing_errors =
  let doc = "Parsing errors do not abort analysis" in
  Arg.(value & flag & info ["ignore-parsing-errors"] ~doc)

let show_map =
  let doc = "Show the inferred Memory Access Protocol (MAP)." in
  Arg.(value & flag & info ["show-map"] ~doc)


let pico_t = Term.(
  const pico
  $ get_fname
  $ block_dim
  $ grid_dim
  $ use_maxima
  $ use_absynth
  $ use_cofloco
  $ use_koat
  $ show_ra
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
  $ output_json
  $ only_reads
  $ only_writes
  $ params
  $ approx_ifs
  $ strategy
  $ metric
  $ compact
  $ ignore_parsing_errors
  $ show_map
)

let info =
  let doc = "Static analysis of bank-conflicts for GPU programs" in
  Cmd.info "faial-cost" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
