open Stage0
open Inference

let analyze (j:Yojson.Basic.t) : unit =
  match C_lang.parse_program j with
  | Ok k1 ->
    let k2 = D_lang.rewrite_program k1 in
       Indexflow.types_program k2
       |> List.iter (fun (name, d) ->
        print_endline (name ^ "," ^ Indexflow.Stmt.to_string d)
      )

  | Error e ->
    prerr_endline ("Error parsing file");
    Rjson.print_error e;
    exit(-1)

let main (fname: string) : unit =
  fname
  |> Cu_to_json.cu_to_json ~ignore_fail:true
  |> analyze 

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Data-dependency analysis for GPU programs" in
  Cmd.info "data-dep" ~version:"%%VERSION%%" ~doc


let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

