open Inference
open Protocols

let analyze ~ignore_parsing_errors (fname:string): unit =
  let k =
    Protocol_parser.Silent.to_proto
      ~abort_on_parsing_failure:(not ignore_parsing_errors)
      fname
  in
  k.kernels |> List.iter (fun k ->
    let k = Proto.Kernel.apply_arch Architecture.Block k in
    if not (Proto.Kernel.is_global k) then () else
    let open Proto in
    let env = Variable.Set.union (Params.to_set k.global_variables) Variable.tid_var_set in
    let p = Code.Cond (k.pre, k.code) in
    let to_s k : bool -> string =
      function
      | true -> "ind"
      | false -> k
    in
    let ci = Typing.is_control_independent env p |> to_s "ctrl" in
    let di = Typing.is_data_independent env p |> to_s "data" in
    print_endline (k.name ^ "," ^ di ^ "," ^ ci)
  )

let main
  (fname: string)
  (ignore_parsing_errors:bool)
:
  unit
=
  analyze ~ignore_parsing_errors fname

open Cmdliner

let get_fname =
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let ignore_parsing_errors =
  let doc = "Ignore parsing errors." in
  Arg.(value & flag & info ["ignore-parsing-errors"] ~doc)

let main_t = Term.(const main $ get_fname $ ignore_parsing_errors)

let info =
  let doc = "Data-dependency analysis for GPU programs" in
  Cmd.info "data-dep" ~version:"%%VERSION%%" ~doc


let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit
