(* open Feather.Infix *)

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

let _read_dir (d: Fpath.t) : Fpath.t list =
  Sys.readdir (Fpath.to_string d)
  |> Array.map Fpath.v
  |> Array.to_list

let faial_drf_path : Fpath.t = from_string "../../drf/bin/main.exe"

let faial_drf ?(args=[]) (fname:Fpath.t) : Feather.cmd =
  Feather.process (Fpath.to_string faial_drf_path) (args @ [fname |> Fpath.to_string])

let files = [
  "saxpy-parse-gv.cu", [], 0;
  "saxpy-buggy.cu", [], 1;
  "saxpy-buggy.cu", ["--logic"; "QF_AUFBV"], 1;
]

let () =
  let open Feather in
  let open Fpath in
(*   let open Fpath in *)
  files
  |> List.iter (fun (filename, args, expected_status) ->
    let given = faial_drf ~args (v filename) |> collect everything in
    if given.status <> expected_status then (
      print_endline (given.stdout);
      let args = if args = [] then "" else (String.concat " " args ^ " ") in
      print_endline ("ERROR: faial-drf " ^ args ^ filename);
      print_endline ("Expected status: " ^ string_of_int expected_status ^ " but given: " ^ string_of_int given.status);
      exit 1
    ) else ()
  );
  ()
(*   process drf []  |> run *)
