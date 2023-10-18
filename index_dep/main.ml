open Inference
open Protocols

let analyze ~only_global (fname:string): unit =
  let k = Protocol_parser.Silent.to_proto ~abort_on_parsing_failure:false fname in
  k.kernels |> List.iter (fun k ->
    if only_global && not (Proto.Kernel.is_global k) then () else
    let open Proto in
    let env = Variable.Set.union k.global_variables Variable.tid_var_set in
    let p = Code.Cond (k.pre, k.code) in
    let to_s k : bool -> string =
      function
      | true -> "ind"
      | false -> k
    in
    let ci = Typing.is_control_exact env p |> to_s "ctrl" in
    let di = Typing.is_data_exact env p |> to_s "data" in
    print_endline (k.name ^ "," ^ di ^ "," ^ ci)
  )

let main (fname: string) (only_global:bool) : unit =
  analyze ~only_global fname

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
