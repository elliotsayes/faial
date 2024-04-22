open Stage0

(* -------- Define the actual tests: ------------- *)

let tests = [
  "setRowReadRow.cu", ["--per-request"; "--only-reads"], "1";
  "setRowReadRow.cu", ["--per-request"; "--only-writes"], "1";
  "setColReadCol.cu", ["--per-request"; "--only-reads"], "32";
  "setColReadCol.cu", ["--per-request"; "--only-writes"], "32";
  "setRowReadCol.cu", ["--per-request"; "--only-reads"], "32";
  "setRowReadCol.cu", ["--per-request"; "--only-writes"], "1";
  "setRowReadColPad.cu", ["--per-request"; "--only-reads"], "1";
  "setRowReadColPad.cu", ["--per-request"; "--only-writes"], "1";
  "2tid.cu", ["--blockDim=1024"; "--gridDim=2";], "1";
  "4tid.cu", ["--blockDim=1024"; "--gridDim=1";], "3";
  "6tid.cu", ["--blockDim=1024"; "--gridDim=1";], "1";
  "8tid.cu", ["--blockDim=1024"; "--gridDim=1";], "7";
  "10tid.cu", ["--blockDim=1024"; "--gridDim=1"], "1";
  "12tid.cu", ["--blockDim=1024"; "--gridDim=1"], "3";
  "14tid.cu", ["--blockDim=1024"; "--gridDim=1"], "1";
  "16tid.cu", ["--blockDim=1024"; "--gridDim=1"], "15";
  "18tid.cu", ["--blockDim=1024"; "--gridDim=1"], "1";
  "20tid.cu", ["--blockDim=1024"; "--gridDim=1"], "3";
  "22tid.cu", ["--blockDim=1024"; "--gridDim=1"], "1";
  "24tid.cu", ["--blockDim=1024"; "--gridDim=1"], "7";
  "26tid.cu", ["--blockDim=1024"; "--gridDim=1"], "1";
  "28tid.cu", ["--blockDim=1024"; "--gridDim=1"], "3";
  "30tid.cu", ["--blockDim=1024"; "--gridDim=1"], "1";
  "32tid.cu", ["--blockDim=1024"; "--gridDim=1"], "31";
  "loops-1.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} 1";
  "loops-1-step-plus.cu", [], "Σ_{j | 0 ≤ j ≤ ⌊(ub - lb) / step⌋} 1";
  "loops-1-step-minus.cu", [], "Σ_{j | 0 ≤ j ≤ ⌊(ub - lb) / step⌋} 1";
  "loops-1-step-minus.cu", ["-p"; "lb=2"; "-p"; "ub=33"; "-p"; "step=5"], "7";
  "loops-1-div.cu", [], "9";
  "loops-1-bitwise.cu", [], "Σ_{i | 0 ≤ i ≤ ((n << m) - 1)} 1";
  "loops-1-minus.cu", [], "Σ_{i | 0 ≤ i ≤ n} 1";
  "loops-1-tid.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} 1";
  "loops-1-multiple.cu", [], "Σ_{i | 0 ≤ i ≤ 0} (1 + (3 + 7))";
  "loops-1-pow.cu", ["--block-dim"; "512"], "8";
  "loops-1-pow-2.cu", [], "9";
  "loops-1-pow-unknown-bound.cu", [], "Σ_{i | 1 ≤ i ≤ ⌊log₂((n - 1))⌋} 1";
  "loops-1-pow-unknown-bound.cu", ["-p"; "n=1025"], "10";
  "loops-1-rsh.cu", [], "9";
  "loops-nested-2.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | i ≤ j ≤ (n - 1)} 1";
  "loops-nested-2-ind.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | 0 ≤ j ≤ (n - 1)} 1";
  "loops-nested-2-tid.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | i ≤ j ≤ (n - 1)} 1";
  "loops-nested-2-pow.cu", [], "Σ_{i | 0 ≤ i ≤ (n - 1)} Σ_{j | 1 ≤ j ≤ ⌊log₂((n - 1))⌋} 1";
  "loops-nested-2-ind-step.cu", [], "Σ_{i | 0 ≤ i ≤ ⌊(n - 1) / step1⌋} Σ_{j | 0 ≤ j ≤ ⌊((m - 1) - (i * step1)) / step2⌋} 1";
]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list =
  [
    "example.cu";
    "2tid-racuda-1.cu";
    "2tid-seq-2.cu";
    "blockDim-tid.cu";
    "data-dep.cu";
    "empty.cu";
    "ifs-1.cu";
    "ifs-seq-3.cu";
    "loops-1-tid-v2.cu";
    "loops-nested-3.cu";
    "loops-nested-4.cu";
    "loops-nested-5.cu";
    "loops-seq-2-same-var.cu";
    "loops-seq-2.cu";
    "loops-seq-3.cu";
    "loops-seq-4.cu";
    "loops-seq-5.cu";
    "loops-seq-6.cu";
    "nd-2.cu";
    "nd-3.cu";
    "racuda-index-analysis-bug.cu";
    "tid-tid.cu";
    "tidx-tidy-tidz.cu";
    "types-1.cu";
    "2tid-plus-const.cu";
    "2tid-racuda-1.cu";
    "2tid-racuda-2.cu";
    "2tid-racuda-3.cu";
  ] |> List.map Fpath.v

let faial_bc_path : Fpath.t = Files.from_string "../../bank_conflicts/bin/main.exe"

let faial_bc ?(args=[]) (fname:Fpath.t) : Subprocess.t =
  Subprocess.make
    (Fpath.to_string faial_bc_path)
    (args @ [fname |> Fpath.to_string])

let used_files : Fpath.Set.t =
  tests
  (* get just the filenames as paths *)
  |> List.map (fun (x, _, _) -> Fpath.v x)
  (* convert to a set *)
  |> Fpath.Set.of_list

let missed_files (dir:Fpath.t) : Fpath.Set.t =
  let all_cu_files : Fpath.Set.t =
    dir
    |> Files.read_dir
    |> List.filter (Fpath.has_ext ".cu")
    |> Fpath.Set.of_list
  in
  let unsupported = Fpath.Set.of_list unsupported in
  Fpath.Set.diff (Fpath.Set.diff all_cu_files used_files) unsupported

let () =
  let open Fpath in
  print_endline "Checking bank-conflicts examples:";
  tests
  |> List.iter (fun (filename, args, expected_output) ->
    flush_all ();
    let args = "--only-cost" :: args in
    let str_args = if args = [] then "" else (String.concat " " args ^ " ") in
    let bullet = " - " in
    print_string (bullet ^ "faial-bc " ^ str_args ^ filename);
    Stdlib.flush_all ();
    let given = faial_bc ~args (v filename) |> Subprocess.run_split in
    let expected_output = expected_output |> String.trim in
    let given_output = given.stdout |> String.trim in
    (if given.status = Unix.WEXITED 0 && expected_output = given_output then (
      print_endline " ✔";
    ) else (
      print_endline " ✘";
      print_endline ("----------------------- EXPECTED -----------------------");
      print_endline (expected_output);
      print_endline ("------------------------ GIVEN -------------------------");
      print_endline (given_output);
      print_endline ("----------------------- STDERR -------------------------");
      print_endline (given.stderr);
      exit 1
    ));
    Stdlib.flush_all ();
  );
  unsupported |> List.iter (fun f ->
    if not (Files.exists f) then (
      print_endline ("Missing unsupported file: " ^ Fpath.to_string f);
      exit 1
    ) else (
      print_endline ("TODO:  " ^ Fpath.to_string f);
    )
  );
  let missed = missed_files (v ".") in
  if not (Fpath.Set.is_empty missed) then (
    let missed =
      missed
      |> Fpath.Set.to_list
      |> List.sort Fpath.compare
      |> List.map Fpath.to_string
      |> String.concat " "
    in
    print_endline ("");
    print_endline ("ERROR: The following files are not being checked: " ^ missed);
  ) else
    ()
