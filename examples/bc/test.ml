open Stage0

(* -------- Define the actual tests: ------------- *)

let tests = [
  "setRowReadRow.cu", ["--per-request"; "--only-cost"; "--only-reads"], "1";
  "setRowReadRow.cu", ["--per-request"; "--only-cost"; "--only-writes"], "1";
  "setColReadCol.cu", ["--per-request"; "--only-cost"; "--only-reads"], "32";
  "setColReadCol.cu", ["--per-request"; "--only-cost"; "--only-writes"], "32";
  "setRowReadCol.cu", ["--per-request"; "--only-cost"; "--only-reads"], "32";
  "setRowReadCol.cu", ["--per-request"; "--only-cost"; "--only-writes"], "1";
  "setRowReadColPad.cu", ["--per-request"; "--only-cost"; "--only-reads"], "1";
  "setRowReadColPad.cu", ["--per-request"; "--only-cost"; "--only-writes"], "1";
  "2tid.cu", ["--blockDim=1024"; "--gridDim=2"; "--only-cost"], "1";
  "4tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "3";
  "6tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "8tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "7";
  "10tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "12tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "3";
  "14tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "16tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "15";
  "18tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "20tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "3";
  "22tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "24tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "7";
  "26tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "28tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "3";
  "30tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "1";
  "32tid.cu", ["--blockDim=1024"; "--gridDim=1"; "--only-cost"], "31";
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
    "loops-1.cu";
    "loops-1-bitwise.cu";
    "loops-1-step.cu";
    "loops-1-div.cu";
    "loops-1-minus.cu";
    "loops-1-tid.cu";
    "loops-1-multiple.cu";
    "loops-1-pow.cu";
    "loops-1-rsh.cu";
    "loops-1-tid-v2.cu";
    "loops-1-pow-2.cu";
    "loops-1-pow-unknown-bound.cu";
    "loops-nested-2.cu";
    "loops-nested-2-ind.cu";
    "loops-nested-2-pow.cu";
    "loops-nested-2-tid.cu";
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
