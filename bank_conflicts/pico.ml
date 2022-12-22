open Stage0
open Inference
open Bankconflicts

(* Main function *)

let pico (fname : string) (thread_count:Vec3.t) (use_maxima:bool) =
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let c_ast = parsed_json |> C_lang.parse_program |> Result.get_ok in
    let d_ast = c_ast |> D_lang.rewrite_program in
    let imp = d_ast |> D_to_imp.parse_program |> Result.get_ok in
    let proto = imp |> List.map Imp.compile in
    List.iter (fun k ->
      let cost_of_proto = Bankconflicts.cost ~use_maxima thread_count k in
      print_string (k.kernel_name ^ ":\n");
      cost_of_proto |> print_endline
    ) proto
  with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)

(* Command-line interface *)

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let vec3 : Vec3.t Cmdliner.Arg.conv =
  let parse =
    fun s ->
    try
      match Yojson.Basic.from_string s with
      | `List [`Int x; `Int y; `Int z] -> Ok (Vec3.make ~x ~y ~z)
      | `List [`Int x; `Int y] -> Ok (Vec3.make ~x ~y ~z:1)
      | `List [`Int x] | `Int x -> Ok (Vec3.make ~x:x ~y:1 ~z:1)
      | _ -> Error (`Msg "Expecting a number of a list of up to 3 numbers (eg, [x,y,z])")
    with
      _ -> Error (`Msg ("Error parsing vec3"))
  in
  let print : Vec3.t Cmdliner.Arg.printer =
    fun ppf v -> Format.fprintf ppf "%s" (Vec3.to_string v)
  in
  Arg.conv (parse, print)

let thread_count =
  let doc = "Set the number of threads per block.\nExamples:\n--blockDim 1024\n--blockDim [16,16]." in
  Arg.(value & opt vec3 (Vec3.make ~x:1024 ~y:1 ~z:1) & info ["b"; "block-dim"; "blockDim"] ~docv:"BLOCK_DIM" ~doc)

let use_maxima =
  let doc = "Use maxima." in
  Arg.(value & flag & info ["use-maxima"] ~doc)

let pico_t = Term.(const pico $ get_fname $ thread_count $ use_maxima)

let info =
  let doc = "Static performance analysis for GPU programs" in
  Cmd.info "pico" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info pico_t
  |> Cmd.eval
  |> exit
