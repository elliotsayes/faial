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
]

(* ----- UTIL ---- *)

let join (x:Fpath.t list) : Fpath.t =
  match x with
  | [] -> failwith "empty list"
  | [x] -> x
  | x :: l ->
    List.fold_left Fpath.append x l

let from_list (x:string list) : Fpath.t =
  List.map Fpath.v x |> join

(* cross platform load path, expects path separator to be / *)
let from_string (x:string) : Fpath.t =
  x |> String.split_on_char '/' |> from_list

let read_dir (dir:Fpath.t) : Fpath.t list =
  Sys.readdir (Fpath.to_string dir)
  |> Array.map Fpath.v
  |> Array.to_list

(* ---- Testing-specific code ----- *)

let faial_drf_path : Fpath.t = from_string "../../drf/bin/main.exe"

let faial_drf ?(args=[]) (fname:Fpath.t) : Feather.cmd =
  Feather.process (Fpath.to_string faial_drf_path) (args @ [fname |> Fpath.to_string])

let used_files : Fpath.Set.t =
  tests
  (* get just the filenames as paths *)
  |> List.map (fun (x, _, _) -> Fpath.v x)
  (* convert to a set *)
  |> Fpath.Set.of_list

let missed_files (dir:Fpath.t) : Fpath.Set.t =
  let all_cu_files : Fpath.Set.t =
    dir
    |> read_dir
    |> List.filter (Fpath.has_ext ".cu")
    |> Fpath.Set.of_list
  in
  Fpath.Set.diff all_cu_files used_files

let () =
  let open Feather in
  let open Fpath in
  print_endline "Checking examples for DRF:";
  tests
  |> List.iter (fun (filename, args, expected_status) ->
    let str_args = if args = [] then "" else (String.concat " " args ^ " ") in
    print_string ("- faial-drf " ^ str_args ^ filename);
    Stdlib.flush_all ();
    let given = faial_drf ~args (v filename) |> collect everything in
    (if given.status <> expected_status then (
      print_endline " ✘";
      print_endline (given.stdout);
      print_endline ("ERROR: Expected status: " ^ string_of_int expected_status ^ " but given: " ^ string_of_int given.status);
      exit 1
    ) else (
      print_endline " ✔";
    ));
    Stdlib.flush_all ();
  );
  let missed = missed_files (v ".") in
  if not (Fpath.Set.is_empty missed) then (
    let missed =
      missed
      |> Fpath.Set.to_list
      |> List.sort Fpath.compare
      |> List.map Fpath.to_string
      |> String.concat ", "
    in
    print_endline ("");
    print_endline ("ERROR: The following files are not being checked: " ^ missed);
  ) else
    ()
