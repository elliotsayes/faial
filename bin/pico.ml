
(* Main function *)

let pico (fname : string) =
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
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
