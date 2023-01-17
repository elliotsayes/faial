open Stage0
open Inference
open Bank_conflicts

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
    (thread_count : Vec3.t)
  :
    unit
  =
  let prepare_kernel = Prep.prepare_kernel racuda thread_count in
  let kernels = read_kernels input_file |> List.map prepare_kernel in
  let generator = if toml then Tgen.gen_toml racuda else Cgen.gen_cuda racuda in
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

let vec3 : Vec3.t Cmdliner.Arg.conv =
  let parse =
    fun s ->
      try
        match Yojson.Basic.from_string s with
        | `List [`Int x; `Int y; `Int z] -> Ok (Vec3.make ~x ~y ~z)
        | `List [`Int x; `Int y] -> Ok (Vec3.make ~x ~y ~z:1)
        | `List [`Int x] | `Int x -> Ok (Vec3.make ~x:x ~y:1 ~z:1)
        | _ -> Error (`Msg ("Expecting a number of a list of " ^
                            " up to 3 numbers (eg, [x,y,z])"))
      with
        _ -> Error (`Msg ("Error parsing vec3"))
  in
  let print : Vec3.t Cmdliner.Arg.printer =
    fun ppf v -> Format.fprintf ppf "%s" (Vec3.to_string v)
  in
  Arg.conv (parse, print)

let thread_count =
  let doc = "Set the number of threads per block. " ^
            "Examples: --blockDim 1024\n--blockDim [16,16]. " ^
            "Only used if --racuda flag is enabled."
  in
  Arg.(value & opt vec3 (Vec3.make ~x:1024 ~y:1 ~z:1) &
       info ["b"; "block-dim"; "blockDim"] ~docv:"BLOCK_DIM" ~doc)

let corvo_t = Term.(
    const corvo
    $ input_file
    $ output_file
    $ racuda
    $ toml
    $ thread_count
  )

let info =
  let doc = "Generates CUDA code from a protocol" in
  Cmd.info "faial-gen" ~doc

let () =
  Cmd.v info corvo_t
  |> Cmd.eval
  |> exit
