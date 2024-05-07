open Stage0
open Protocols
open Inference
open Bank_conflicts

let load_data (fname : string) : (string * int) list =
   try
    let open Yojson.Basic in
    let j = from_file ~fname fname in
    j
    |> Util.to_assoc
    |> List.filter_map (fun (k, v) ->
      v
      |> Util.to_int_option
      |> Option.map (fun v -> (k, v))
    )
  with
    | Yojson.Json_error e | Sys_error e ->
      prerr_endline ("Error parsing '" ^ fname ^ "': " ^ e);
      []


let shared_arrays (k:Proto.Code.t Proto.Kernel.t) : Variable.Set.t =
  Variable.Map.bindings k.arrays
  |> List.filter_map (fun (k, a) ->
    if Memory.is_shared a then
      Some k
    else
      None
  )
  |> Variable.Set.of_list

let create_ctx ~bank_count ~env:(env:(string*int) list) ~arrays : Vectorized.t =
  let use_array x = Variable.Set.mem x arrays in
  let block_dim =
    let x = List.assoc_opt "blockDim.x" env |> Option.value ~default:32 in
    let y = List.assoc_opt "blockDim.y" env |> Option.value ~default:1 in
    let z = List.assoc_opt "blockDim.z" env |> Option.value ~default:1 in
    Dim3.{x; y; z}
  in
  print_endline (Dim3.to_string block_dim);
  let ctx =
    Vectorized.make ~bank_count ~warp_count:bank_count ~use_array
    |> Vectorized.put_tids block_dim
  in
  print_endline (Vectorized.to_string ctx);
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
    let proto = imp |> List.map Imp.Kernel.compile in
    let env = load_data "env.json" in
    print_string "env.json:";
    print_endline (
      env
      |> List.map (fun (x, y) -> x ^ "=" ^ string_of_int y)
      |> String.concat ", "
    );
    (try
      List.iter (fun p ->
        let ctx = create_ctx ~bank_count:32 ~env ~arrays:(shared_arrays p) in
        let cost = Vectorized.eval p.code ctx in
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
