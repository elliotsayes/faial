open Stage0
open Inference
open Bank_conflicts
open Protocols

(* Parses GPUVerify arguments from the CUDA file *)
let read_params (fname : string) : Gv_parser.t * Params.t =
  let gv = match Gv_parser.parse fname with
    | Some gv ->
      Logger.Colors.info ("Found GPUVerify args in source file: "
                          ^ Gv_parser.to_string gv);
      gv
    | None -> Gv_parser.default

  in
  let block_dim = gv.block_dim in
  let grid_dim = gv.grid_dim in
  gv, Params.make ~block_dim ~grid_dim ()

(* Parses a list of protocols from the CUDA file *)
let read_kernels (fname : string) : Proto.prog Proto.kernel list =
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

(* Generates the output file *)
let write_string (fname : string) (data : string) : unit =
  let oc = open_out fname in
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
  let gv, params = read_params input_file in
  let prepare_kernel = Prep.prepare_kernel racuda params in
  let kernels = read_kernels input_file |> List.map prepare_kernel in
  let generator =
    if toml then Tgen.gen_toml racuda gv else Cgen.gen_cuda racuda gv
  in
  List.map generator kernels |> Common.join "\n" |> write_string output_file;
  if racuda then Cgen.gen_params gv |> write_string (output_file ^ ".params");

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
