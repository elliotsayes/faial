open Stage0
open Inference

(* For parsing the CUDA file *)
let read_kernels (fname : string) =
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let c_ast = parsed_json |> C_lang.parse_program |> Result.get_ok in
    let d_ast = c_ast |> D_lang.rewrite_program in
    let imp = d_ast |> D_to_imp.Silent.parse_program |> Result.get_ok in
    imp |> List.map Imp.compile
  with
  | Common.ParseError b ->
    Buffer.output_buffer stderr b;
    exit (-1)

(* For generating the output file *)
let write_string (filename : string) (data : string) : unit =
  let oc = open_out filename in
  try
    output_string oc data;
    close_out oc
  with ex ->
    close_out oc;
    raise ex

(* Main function *)
let corvo
    (input_file : string)
    (output_file : string)
    (racuda : bool)
    (toml : bool)
  : unit =
  let open Cgen in
  let kernels = match read_kernels input_file, racuda with
    | kernels, true -> List.map mk_racuda_friendly kernels
    | kernels, false -> kernels
  in
  let generator = if toml then gen_toml racuda else gen_cuda racuda in
  List.map generator kernels |> Common.join "\n" |> write_string output_file

open Cmdliner

(* Command-line interface *)
let input_file =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let output_file =
  let doc = "The path of the output file." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"OUTPUT" ~doc)

let racuda =
  let doc = "Generate a RaCUDA-friendly kernel." in
  Arg.(value & flag & info ["r"; "racuda"] ~doc)

let toml =
  let doc = "Generate a TOML file." in
  Arg.(value & flag & info ["t"; "toml"] ~doc)

let corvo_t = Term.(
    const corvo
    $ input_file
    $ output_file
    $ racuda
    $ toml
  )

let info =
  let doc = "Generates CUDA code from a protocol" in
  Cmd.info "faial-gen" ~doc

let () =
  Cmd.v info corvo_t
  |> Cmd.eval
  |> exit
