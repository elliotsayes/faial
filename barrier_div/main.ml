open Inference

(* module T = ANSITerminal *)

let main
  (fname: string)
  (ignore_parsing_errors:bool)
:
  unit
=
  let parsed =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  parsed.kernels
  |> List.iter (fun k ->
    let k = Barrier.Kernel.from_proto k in
    let is_unif = Barrier.Kernel.is_uniform k in
    print_endline (k.name ^ ": " ^ (if is_unif then "true" else "false"))
  )


open Cmdliner

let get_fname : string Term.t =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let ignore_parsing_errors : bool Term.t =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info ["ignore-parsing-errors"] ~doc)

let main_t : unit Term.t = Term.(
  const main
  $ get_fname
  $ ignore_parsing_errors
)

let info =
  let doc = "Check for barrier divergence errors" in
  Cmd.info "faial-sync" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

