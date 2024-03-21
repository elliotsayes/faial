open Stage0

(* -------- Define the actual tests: ------------- *)

let tests = [
  (* The example should be DRF *)
  "parse-gv.cu", [], 0;
  (* Unless we override the parameters with something other than
     what is in the source code. *)
  "parse-gv.cu", ["--gridDim=3"], 1;
  (* This is the simplest data-race. *)
  "saxpy-racy.cu", [], 1;
  (* Sanity check, make sure that the bit-vector logic works. *)
  "saxpy-racy.cu", ["--logic"; "QF_AUFBV"], 1;
  (* This is the simplest data-race free example. *)
  "saxpy.cu", [], 0;
  (* This example is only racy at the grid-level *)
  "racy-grid-level.cu", [], 0;
  "racy-grid-level.cu", ["--grid-level"], 1;
  (* This is a data-race in a 2D shared array. *)
  "racy-2d.cu", [], 1;
  (* This is a data-race on a shared scalar. *)
  "racy-shared-scalar.cu", [], 1;
  (* A data-race in shared memory is invisible at the grid level. *)
  "racy-shared-scalar.cu", ["--grid-level"], 0;
  (* A data-race free example that relies on top-level assignments. *)
  "toplevel-drf.cu", [], 0;
  (* A data-race that occurs when analysis understand top-level assignments.
     We ensure it's a data-race between threads 0 and 1. *)
  "toplevel-racy.cu", ["--tid1"; "0"; "--tid2"; "1"], 1;
  (* Data-race free example *)
  "drf-shared-mem.cu", [], 0;
  (* Data-race free example *)
  "racy-shared-mem.cu", [], 1;
  (* Data-race free example with array aliasing *)
  "drf-alias.cu", [], 0;
  (* Data-race free example with array aliasing *)
  "racy-alias.cu", [], 1;
  (* Data-race freee requires understanding fields in parameters. *)
  "drf-field-in-param.cu", [], 0;
  (* Data-race with atomics. *)
  "racy-atomics.cu", [], 1;
  (* A data-race that occurs when we have warp-concurrent semantics *)
  "racy-reduce.cu", [], 1;
  (* A data-race free example as long as the analysis understands typedefs. *)
  "drf-typedef.cu", [], 0;
  (* The running example of CAV21 *)
  "racy-cav21.cu", [], 1;
  (* The fixed running example of CAV21 *)
  "drf-cav21.cu", [], 0;
  (* A racy example *)
  "racy-device.cu", [], 1;
  (* A data-race that uses aliasing and templated arrays *)
  "racy-template-alias.cu", [], 1;
  (* A data-race that uses aliasing and templated arrays *)
  "racy-template.cu", [], 1;
  (* Improve the support for creating decls due to mutation *)
  "racy-mutation.cu", [], 1;
  (* Support for enumerates *)
  "drf-enum.cu", [], 0;
  (* Support for enumerates *)
  "drf-enum-constraint.cu", [], 0;
]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list =
  [
    "drf-warp.cu";
    "racy-warp.cu";
  ] |> List.map Fpath.v

(* ---- Testing-specific code ----- *)

let faial_drf_path : Fpath.t = Files.from_string "../../drf/bin/main.exe"

let faial_drf ?(args=[]) (fname:Fpath.t) : Subprocess.t =
  Subprocess.make (Fpath.to_string faial_drf_path) (args @ [fname |> Fpath.to_string])

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
  print_endline "Checking examples for DRF:";
  tests
  |> List.iter (fun (filename, args, expected_status) ->
    let str_args = if args = [] then "" else (String.concat " " args ^ " ") in
    let bullet =
      match expected_status with
      | 0 -> "DRF:  "
      | 1 -> "RACY: "
      | _ -> "?:    "
    in
    print_string (bullet ^ "faial-drf " ^ str_args ^ filename);
    Stdlib.flush_all ();
    let given = faial_drf ~args (v filename) |> Subprocess.run_split in
    (if given.status = Unix.WEXITED expected_status then (
      print_endline " ✔";
    ) else (
      print_endline " ✘";
      print_endline (given.stdout);
      print_endline ("ERROR: Expected status: " ^ string_of_int expected_status ^ " but given: " ^ Subprocess.Completed2.to_string given);
      exit 1
    ));
    Stdlib.flush_all ();
  );
  unsupported |> List.iter (fun f ->
    if not (Files.exists f) then (
      print_endline ("Missing unsupported file: " ^ Fpath.to_string f);
      exit 1
    ) else (
      print_endline ("TODO: " ^ Fpath.to_string f);
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
