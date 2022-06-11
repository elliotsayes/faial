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

let pico (fname : string) =
  try
    let raw_json = cu_to_json fname in
    let parsed_json = parse_json raw_json in
    let c_ast = parsed_json |> Cast.parse_program |> Result.get_ok in
    let d_ast = c_ast |> Dlang.rewrite_program in
    let imp = d_ast |> D_to_imp.parse_program |> Result.get_ok in
    let proto = imp |> List.map Imp.compile in
    let cost_of_proto = Bankconflicts.p_k_cost proto in
    cost_of_proto |> Serialize.PPrint.n_to_s |> print_endline
  with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)

(* Command-line interface *)

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let pico_t = Term.(const pico $ get_fname)

let info =
  let doc = "Static performance analysis for GPU programs" in
  Term.info "pico" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (pico_t, info)
