open Cmdliner

(* For parsing CUDA files *)
let parse_cuda (fname : string) =
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let c_ast = parsed_json |> Cast.parse_program |> Result.get_ok in
    let d_ast = c_ast |> Dlang.rewrite_program in
    let imp = d_ast |> D_to_imp.parse_program |> Result.get_ok in
    imp |> List.map Imp.compile
  with
  | Common.ParseError b ->
    Buffer.output_buffer stderr b;
    exit (-1)

(* For parsing protocols *)
let parse_proto (fname : string) =
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

(* Main function *)
let read_kernels (fname : string) (use_cuda : bool) =
  if use_cuda then parse_cuda fname
  else parse_proto fname

(* Command-line interface *)
let p2c_t =
  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)
  in
  let use_cuda =
    let doc = "Parse a CUDA file" in
    Arg.(value & flag & info ["c"; "cuda"] ~doc)
  in
  let racuda =
    let doc = "Generate a RaCUDA-friendly kernel" in
    Arg.(value & flag & info ["r"; "racuda"] ~doc)
  in
  let output_toml =
    let doc = "Output a TOML file" in
    Arg.(value & flag & info ["t"; "toml"] ~doc)
  in
  let do_p2c
      (fname : string)
      (use_cuda : bool)
      (racuda : bool)
      (output_toml : bool) : unit =
    let open Cgen in
    let kernels = read_kernels fname use_cuda in
    if output_toml then
      List.iter (fun k -> print_t k racuda) kernels
    else
      List.iter (fun k -> print_k k racuda) kernels
  in
  Term.(
    const do_p2c
    $ get_fname
    $ use_cuda
    $ racuda
    $ output_toml
  )

let info =
  let doc = "Generates a CUDA file from a protocol" in
  Term.info "proto-to-cuda" ~doc ~exits:Term.default_exits

(* ----------------- execution entry point -------------------- *)

let _ =
  Term.eval (p2c_t, info)
  |> Term.exit
