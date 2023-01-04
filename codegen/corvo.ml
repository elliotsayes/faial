open Stage0
open Inference

open Cmdliner

(* For parsing CUDA files *)
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

(* Command-line interface *)
let p2c_t =
  let get_fname =
    let doc = "The path $(docv) of the GPU program." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)
  in
  let racuda =
    let doc = "Generate a RaCUDA-friendly kernel." in
    Arg.(value & flag & info ["r"; "racuda"] ~doc)
  in
  let output_toml =
    let doc = "Output a TOML file." in
    Arg.(value & flag & info ["t"; "toml"] ~doc)
  in
  let do_p2c
      (fname : string)
      (racuda : bool)
      (output_toml : bool) : unit =
    let open Cgen in
    let kernels = read_kernels fname in
    if output_toml then
      List.iter (fun k -> print_t k racuda) kernels
    else
      List.iter (fun k -> print_k k racuda) kernels
  in
  Term.(
    const do_p2c
    $ get_fname
    $ racuda
    $ output_toml
  )

let info =
  let doc = "Generates a CUDA file from a protocol" in
  Cmd.info "proto-to-cuda" ~doc

(* ----------------- execution entry point -------------------- *)

let () =
  Cmd.v info p2c_t
  |> Cmd.eval
  |> exit
