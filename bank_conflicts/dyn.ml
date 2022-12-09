open Stage0
open Protocols
open Inference

let load_data (fname : string) : (string * int) list =
  try
    let open Yojson.Basic in
    let j = from_file fname in
    j
    |> Util.to_assoc
    |> List.filter_map (fun (k, v) ->
      v
      |> Util.to_int_option
      |> Option.map (fun v -> (k, v))
    )
  with
    _ -> []

let create_ctx ~bank_count ~tid_count ~env:(env:(string*int) list) : Vectorized.t =
  List.fold_left (fun ctx ((k:string), (v:int)) ->
    print_endline (k ^ " = " ^ string_of_int v);
    let k = Variable.from_name k in
    let v = Vectorized.NMap.constant ~count:tid_count ~value:v in
    Vectorized.put k v ctx
  ) (Vectorized.make ~bank_count ~tid_count) env

(* Main function *)

let main (fname : string) : unit =
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let c_ast = parsed_json |> C_lang.parse_program |> Result.get_ok in
    let d_ast = c_ast |> D_lang.rewrite_program in
    let imp = d_ast |> D_to_imp.parse_program |> Result.get_ok in
    let proto = imp |> List.map Imp.compile in
    let env = load_data "env.json" in
    (try
      let ctx = create_ctx ~bank_count:32 ~tid_count:32 ~env in
      List.iter (fun p ->
        let open Proto in
        let cost = Vectorized.eval p.kernel_code ctx in
        let (bid, max_cost) = Vectorized.NMap.max cost in
        print_endline ("Dynamic cost (bid " ^ string_of_int bid ^ "): " ^ string_of_int max_cost)
      ) proto;
      ()
    with
      | Failure x -> print_endline ("Dynamic analysis failed: " ^ x)
      | _ -> print_endline ("Dynamic analysis failed"))

  with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1)

(* Command-line interface *)

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Dynamic performance analysis for GPU programs" in
  Cmd.info "faial-bc-dyn" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit
