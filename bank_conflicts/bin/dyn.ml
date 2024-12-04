open Stage0
open Protocols
open Inference
open Bank_conflicts

module Data = struct
  open Vectorized

  type t =
    | Int of int
    | Vector of int list

  let from_json (j:Yojson.Basic.t) : t option =
    let open Yojson.Basic.Util in
    match j with
    | `Int i -> Some (Int i)
    | `List l ->
      (try
        Some (Vector (List.filter_map Yojson.Basic.Util.to_int_option l))
      with
        Type_error _ -> None)
    | _ -> None

  let to_int : t -> int =
    function
    | Int i -> i
    | _ -> failwith "to_int"

  let to_nmap ~count : t -> NMap.t option =
    function
    | Int value ->
      Some (NMap.constant ~count ~value)
    | Vector v ->
      if count <> List.length v then
        None
      else
        Some (NMap.from_array (Array.of_list v))

  let to_string : t -> string =
    function
    | Int i -> string_of_int i
    | Vector l ->
      let l = List.map string_of_int l |> String.concat ", " in
      "[" ^ l ^ "]"
end

module Env = struct
  type t = (string * Data.t) list
  let load (fname : string) : t =
    try
      let open Yojson.Basic in
      let j = from_file ~fname fname in
      j
      |> Util.to_assoc
      |> List.filter_map (fun (k, v) ->
        v
        |> Data.from_json
        |> Option.map (fun v -> (k, v))
      )
    with
      | Yojson.Json_error e | Sys_error e ->
        prerr_endline ("Error parsing '" ^ fname ^ "': " ^ e);
        []

  let to_vectorized ~bank_count ~env:(env:t) ~arrays : Vectorized.t =
    let use_array x = Variable.Set.mem x arrays in
    let block_dim =
      let to_v (key:string) (default:int) : int =
        env
        |> List.assoc_opt key
        |> Option.map Data.to_int
        |> Option.value ~default
      in
      let x = to_v "blockDim.x" bank_count in
      let y = to_v "blockDim.y" 1 in
      let z = to_v "blockDim.z" 1 in
      Dim3.{x; y; z}
    in
    print_endline (Dim3.to_string block_dim);
    let ctx =
      Vectorized.make ~bank_count ~thread_count:bank_count ~use_array
      |> Vectorized.put_tids block_dim
    in
    print_endline (Vectorized.to_string ctx);
    List.fold_left (fun ctx ((k:string), v) ->
      let k = Variable.from_name k in
      match Data.to_nmap v ~count:bank_count with
      | Some v' ->
        print_endline (Variable.name k ^ " = " ^ Data.to_string v);
        Vectorized.put k v' ctx
      | None -> ctx
    ) ctx env

  let to_string (env:t) : string =
    let env =
      env
      |> List.map (fun (x, y) -> x ^ "=" ^ Data.to_string y)
      |> String.concat ", "
    in
    "{" ^ env ^ "}"
end


let shared_arrays (k:Protocols.Kernel.t) : Variable.Set.t =
  Variable.Map.bindings k.arrays
  |> List.filter_map (fun (k, a) ->
    if Memory.is_shared a then
      Some k
    else
      None
  )
  |> Variable.Set.of_list


let main (fname : string) : unit =
  try
    let parsed = Protocol_parser.Default.to_proto fname in
    let proto = parsed.kernels in
    let env = Env.load "env.json" in
    print_endline ("env.json: " ^ Env.to_string env);
    try
      List.iter (fun p ->
        let ctx = Env.to_vectorized ~bank_count:32 ~env ~arrays:(shared_arrays p) in
        let v = Vectorized.eval ~verbose:true Metric.BankConflicts p.code ctx in
        print_endline ("Total cost: " ^ string_of_int v)
      ) proto;
      ()
    with
      | Failure x -> print_endline ("Dynamic analysis failed: " ^ x)


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
