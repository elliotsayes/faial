
let analyze (j:Yojson.Basic.t) : unit =
  let open Indexflow in
  let open Cast in
  let open D_to_imp in
  match Cast.parse_program j with
  | Ok k1 ->
    let k2 = Dlang.rewrite_program k1 in
      let errors = Indexflow.types_program k2
      |> Common.map_opt (fun (x, r) -> 
        match r with
        | Error e -> Some (x, e)
        | Ok _ -> None
      )
      in
      (match errors with
      | [] ->
        print_endline "INDEPENDENT"
      | (_, e) :: _ ->
        Indexflow.print_s_error e;
        prerr_endline "DATA-DEPENDENT";
        exit (-1)
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)

let main (fname: string) : unit =
  fname
  |> Cu_to_json.cu_to_json
  |> analyze 

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Data-dependency analysis for GPU programs" in
  Term.info "data-dep" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (main_t, info)

