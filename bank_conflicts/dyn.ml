open Stage0
open Protocols
open Inference
open Bc

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

let shared_arrays (k:Proto.prog Proto.kernel) : Variable.Set.t =
  let open Proto in
  let open Exp in
  Variable.Map.bindings k.kernel_arrays
  |> List.filter_map (fun (k, a) ->
    if a.array_hierarchy = SharedMemory then
      Some k
    else
      None
  )
  |> Variable.Set.of_list

let create_ctx ~bank_count ~env:(env:(string*int) list) ~arrays : Vectorized.t =
  let use_array x = Variable.Set.mem x arrays in
  let ctx = Vectorized.make ~bank_count ~warp_count:bank_count ~use_array
    |> Vectorized.put_tids (Vec3.make ~x:bank_count ~y:1 ~z:1)
  in
  List.fold_left (fun ctx ((k:string), (v:int)) ->
    print_endline (k ^ " = " ^ string_of_int v);
    let k = Variable.from_name k in
    let v = Vectorized.NMap.constant ~count:bank_count ~value:v in
    Vectorized.put k v ctx
  ) ctx env

let main (fname : string) : unit =
  try
    let parsed_json = Cu_to_json.cu_to_json fname in
    let c_ast = parsed_json |> C_lang.parse_program |> Result.get_ok in
    let d_ast = c_ast |> D_lang.rewrite_program in
    let imp = d_ast |> D_to_imp.Silent.parse_program |> Result.get_ok in
    let proto = imp |> List.map Imp.compile in
    let env = load_data "env.json" in
    (try
      List.iter (fun p ->
        let ctx = create_ctx ~bank_count:32 ~env ~arrays:(shared_arrays p) in
        let open Proto in
        let cost = Vectorized.eval p.kernel_code ctx in
        let v = Vectorized.NMap.max cost in
        print_endline ("Dynamic cost (bid " ^ string_of_int v.index ^ "): " ^ string_of_int v.value)
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
