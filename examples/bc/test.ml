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
]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list = []

let faial_bc_path : Fpath.t = Files.from_string "../../bank_conflicts/bin/main.exe"

let faial_bc ?(args=[]) (fname:Fpath.t) : Subprocess.t =
  Subprocess.make
    (Fpath.to_string faial_bc_path)
    (args @ [fname |> Fpath.to_string])

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
  )
  (*
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
  *)
