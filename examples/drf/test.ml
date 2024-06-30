open Stage0

(* -------- Define the actual tests: ------------- *)

let tests = [
  (* The example should be DRF *)
  "parse-gv.cu", [], 0;
  (* Unless we override the parameters with something other than
     what is in the source code. *)
  "parse-gv.cu", ["--gridDim=3"], 1;
  (* This is the simplest data-race. *)
  "racy-saxpy.cu", [], 1;
  (* Sanity check, make sure that the bit-vector logic works. *)
  "racy-saxpy.cu", ["--logic"; "QF_AUFBV"], 1;
  (* This is the simplest data-race free example. *)
  "drf-saxpy.cu", [], 0;
  (* The kernel contains constraints that makes it DRF: blockDim.{y,z}=1
     and gridDim.{y,z}=1. *)
  "drf-saxpy.cu", ["--all-dims"; "--all-levels"], 0;
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
  "drf-toplevel.cu", [], 0;
  (* A data-race that occurs when analysis understand top-level assignments.
     We ensure it's a data-race between threads 0 and 1. *)
  "racy-toplevel.cu", ["--tid1"; "0"; "--tid2"; "1"], 1;
  (* Data-race free example *)
  "drf-shared-mem.cu", [], 0;
  (* Shared memory *)
  "racy-shared-mem.cu", [], 1;
  (* Shared memory in a device function *)
  "racy-shared-mem-2.cu", [], 1;
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
  (* Support for anonymous enumerates named via typedef *)
  "drf-enum-typedef.cu", [], 0;
  (* Support for enumerates *)
  "drf-enum-constraint.cu", [], 0;
  (* Aliasing using shared memory (example 1) *)
  "racy-alias-shmem1.cu", [], 1;
  (* Aliasing using shared memory (example 1) *)
  "racy-alias-shmem2.cu", [], 1;
  (* Aliasing using shared memory (example 1) *)
  "racy-alias-shmem3.cu", [], 1;
  (* Aliasing with increment *)
  "racy-alias-assign.cu", [], 1;
  (* Array accesses of local memory should not introduce data-races. *)
  "drf-local-array.cu", [], 0;
  (* Check support for macros *)
  "macro.cu", ["-DMACRO="], 0;
  "macro.cu", ["-DMACRO=+ 0"], 0;
  "macro.cu", ["-DMACRO=+ 1"], 1; (* data-race *)
  "macro.cu", ["-DMACRO"], 2; (* expands to 1, which is a syntax error *)
  "macro.cu", [], 2; (* syntax error if the macro is not defined *)
  (* A conditional break is inferred as an assertion *)
  "drf-assert-loop.cu", [], 0;
  (* Bug from generating unknowns from function calls *)
  "racy-funcion-call-unknowns.cu", [], 1;
  (* Bug from generating unknowns from a kernel call *)
  "racy-kernel-calls-return.cu", [], 1;
  (* (int j = 0; j < n; j++) *)
  "drf-loop1.cu", [], 0;
  (* (int i = 0; i <= 4; i++) *)
  "drf-loop3.cu", [], 0;
  (* (int j = n; j >= 0; j--) *)
  "drf-loop2.cu", [], 0;
  (* (int j = n; j > 0; j--) *)
  "drf-loop5.cu", [], 0;
  (* (int i = 4; i - k; i++) *)
  "drf-loop4.cu", [], 0;
  (* (int j = 0; j <= n; j++) *)
  "racy-loop1.cu", ["-p"; "n=0"; "--index=[0]"], 1;
  (* (int j = n; j >= 0; j--) *)
  "racy-loop2.cu", ["-p"; "n=1"; "--index=[1]"], 1;
  (* the comma operator *)
  "racy-comma.cu", [], 1;
  (* the comma operator *)
  "drf-comma.cu", [], 0;
]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list =
  [
    "drf-warp.cu";
    "racy-warp.cu";
    "racy-device-ref.cu";
    (* example where assignment is used as an expression, rather
     than a statement *)
    "drf-assign-exp.cu";
  ] |> List.map (fun x -> Fpath.(v "." / x))

(* ---- Testing-specific code ----- *)

let faial_drf_path : Fpath.t = Files.from_string "../../drf/bin/main.exe"

let faial_drf ?(args=[]) (fname:Fpath.t) : Subprocess.t =
  Subprocess.make (Fpath.to_string faial_drf_path) (args @ [fname |> Fpath.to_string])

let used_files : Fpath.Set.t =
  tests
  (* get just the filenames as paths *)
  |> List.map (fun (x, _, _) -> Fpath.(v "." / x))
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
      | 0 -> "DRF:   "
      | 1 -> "RACY:  "
      | 2 -> "PARSE: "
      | _ -> "?:     "
    in
    print_string (bullet ^ "faial-drf " ^ str_args ^ filename);
    Stdlib.flush_all ();
    let given = faial_drf ~args (v filename) |> Subprocess.run_split in
    (if given.status = Unix.WEXITED expected_status then (
      print_endline " ✔";
    ) else (
      let exit_code = Subprocess.exit_code given.status |> string_of_int in
      print_endline " ✘";
      print_endline ("------------------------ OUTPUT ------------------------");
      print_endline (given.stdout);
      print_endline (given.stderr);
      print_endline ("ERROR: Expected return code " ^ string_of_int expected_status ^ " but got " ^ exit_code);
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
    exit (-1);
  ) else
    ()
