open Cmdliner

(* Parse C AST as JSON using cu-to-json *)
let cu_to_json (fname : string) : string =
  let cmd = "cu-to-json " ^ fname in
  let in_c = Unix.open_process_in cmd in
  let raw_json_ast = In_channel.input_all in_c in
  In_channel.close in_c; raw_json_ast

let parse_json (raw_json_ast : string) : Yojson.Basic.t =
  try Yojson.Basic.from_string raw_json_ast with
  | Yojson.Json_error("Blank input data") ->
    (* If the input is blank, just ignore the input and err with -1 *)
    raise (Common.mk_parse_error_s "Empty input data. Blank file?\n")
  | Yojson.Json_error(e) ->
    raise (Common.mk_parse_error_s (Printf.sprintf "Error parsing JSON: %s\n" e))

(* Main function *)
let read_kernels (fname : string) (use_cuda : bool) =
  if use_cuda then
    (* For parsing CUDA files: *)
    try
      let raw_json = cu_to_json fname in
      let parsed_json = parse_json raw_json in
      let c_ast = parsed_json |> Cast.parse_program |> Result.get_ok in
      let d_ast = c_ast |> Dlang.rewrite_program in
      let imp = d_ast |> D_to_imp.parse_program |> Result.get_ok in
      imp |> List.map Imp.compile
    with
    | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)
  else
    (* For parsing protocols: *)
    let filebuf = Lexing.from_channel (open_in fname) in
    try [Parse2.main Scan.read filebuf] with
    | Parse2.Error ->
      let sloc = Sourceloc.of_lexbuf filebuf in
      let b = Buffer.create 1024 in
      Printf.bprintf b "%a: syntax error\n" Sourceloc.location_bprint_start sloc;
      (try
         Printf.bprintf b "\n%a" Sourceloc.location_bprint_title sloc
       with
         Sys_error _ -> ());
      Buffer.output_buffer stderr b;
      exit (-1)

(* Command-line interface *)
let p2c_t =
  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)
  in
  let use_cuda =
    let doc = "Parse a CUDA file" in
    Arg.(value & flag & info ["cuda"] ~doc)
  in
  let output_toml =
    let doc = "Output a TOML file" in
    Arg.(value & flag & info ["toml"] ~doc)
  in
  let do_p2c
      (fname : string)
      (use_cuda : bool)
      (output_toml : bool) : unit =
    let open Cgen in
    let kernels = read_kernels fname use_cuda in
    if output_toml then
      List.iter (fun k -> print_toml (kernel_to_toml k)) kernels
    else
      List.iter (fun k -> print_k k) kernels
  in
  Term.(
    const do_p2c
    $ get_fname
    $ use_cuda
    $ output_toml
  )

let info =
  let doc = "Generates a CUDA file from a protocol" in
  Term.info "proto-to-cuda" ~doc ~exits:Term.default_exits

(* ----------------- execution entry point -------------------- *)

let _ =
  Term.eval (p2c_t, info)
  |> Term.exit
