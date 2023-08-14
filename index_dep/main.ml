open Stage0
open Inference

let analyze ~only_global (j:Yojson.Basic.t) : unit =
  match C_lang.parse_program j with
  | Ok k1 ->
    let k2 = D_lang.rewrite_program k1 in
       k2
       |> List.filter (
         let open D_lang in
         function
         | Kernel k when Kernel.is_global k || not only_global -> true
         | _ -> false
       )
       |> Indexflow.types_program
       |> List.iter (fun (name, d) ->
        print_endline (name ^ "," ^ Indexflow.Stmt.to_string d)
      )

  | Error e ->
    prerr_endline ("Error parsing file");
    Rjson.print_error e;
    exit(-1)

let main (fname: string) (only_global:bool) : unit =
  fname
  |> Cu_to_json.cu_to_json ~ignore_fail:true
  |> analyze ~only_global

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let only_global =
  let doc = "Only analyze kernels annotated with __global__" in
  Arg.(value & flag & info ["only-global"] ~doc)

let main_t = Term.(const main $ get_fname $ only_global)

let info =
  let doc = "Data-dependency analysis for GPU programs" in
  Cmd.info "data-dep" ~version:"%%VERSION%%" ~doc


let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit
