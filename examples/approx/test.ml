open Stage0

let faial_drf_path : Fpath.t = Files.from_string "../../drf/bin/main.exe"

let data_dep_path : Fpath.t = Files.from_string "../../index_dep/main.exe"

let data_dep (fname:Fpath.t) : Subprocess.t =
  Subprocess.make (Fpath.to_string data_dep_path) [fname |> Fpath.to_string]

let faial_drf (fname:Fpath.t) : Subprocess.t =
  Subprocess.make (Fpath.to_string faial_drf_path) [
    fname |> Fpath.to_string;
    "--show-flat-acc";
  ]

let run ~filename:(filename:string) : string option =
  let (approx, variables) =
    match Common.get_lines ~offset:0 ~count:2 filename with
    | [l1; l2] ->
      (Slice.from_start 2 |> Slice.substring l1 |> String.trim,
       Slice.from_start 2 |> Slice.substring l2 |> String.trim)
    | _ -> failwith "impossible"
  in
  let exe = data_dep (Fpath.v filename) in
  let given = exe |> Subprocess.run_split in
  let output = given.stdout |> String.trim in
  if given.status <> Unix.WEXITED 0 then Some (
    "ERROR RUNNING: " ^ Subprocess.to_string exe ^
    "\nExpecting status 0, but got: " ^ Subprocess.Completed2.to_string given
  ) else
  if output <> approx then Some (
    "Expecting '" ^ approx ^ "' got '" ^ output ^ "'"
  ) else
  let exe = faial_drf (Fpath.v filename) in
  let given = exe |> Subprocess.run_split in
  let output = given.stdout |> Common.string_split_lines in
  let output =
    List.nth output 4
    (* approx locals: _unknown_1; *)
    |> Common.split ':'
    |> Option.get
    (* ('approx locals:', ' _unknown_1;') *)
    |> snd
    (* ' _unknown_1;' *)
    |> String.trim
    (* '_unknown_1;' *)
  in
  (* '_unknown_1' *)
  let output = Slice.from_finish (-1) |> Slice.substring output in
  if variables <> output then Some (
    "Expecting variables '" ^ variables ^ "' but got '" ^ output ^ "'"
  ) else
  None

let () =
  print_endline "Checking examples for approximation analysis:";
  "."
  |> Fpath.v
  |> Files.read_dir
  |> List.filter (Fpath.has_ext ".cu")
  |> List.sort Fpath.compare
  |> List.iter (fun filename ->
    let filename = Fpath.to_string filename in
    print_string (" - " ^ filename);
    Stdlib.flush_all ();
    (match run ~filename with
    | None ->
      print_endline " ✔"
    | Some e ->
      print_endline " ✘\n";
      print_endline e;
      print_endline "";
      exit 1
    );
    Stdlib.flush_all ();
  )
