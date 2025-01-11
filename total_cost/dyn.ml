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
(*
  let to_int : t -> int =
    function
    | Int i -> i
    | _ -> failwith "to_int"
*)
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

  let lookup (var:string) (e:t) : Data.t option =
    List.assoc_opt var e

  let lookup_scalar (var:string) (default:int) (e:t) : int =
    match lookup var e with
    | Some (Int n) -> n
    | _ -> default

  let lookup_dim3 (prefix:string) (default:Dim3.t) (e:t) : Dim3.t =
    let field k default =
      lookup_scalar (prefix ^ "." ^ k) default e
    in
    Dim3.{
      x = field "x" default.x;
      y = field "y" default.y;
      z = field "z" default.z;
    }

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

  let to_vectorized ~bank_count ~env:(env:t) (k:Protocols.Kernel.t) : Vectorized.t =
    (*
    let arrays (m:Metric.t) : Variable.Set.t =
      Variable.Map.bindings k.arrays
      |> List.filter_map (fun (k, a) ->
          match m with
          | BankConflicts ->
            if Memory.is_shared a then
              Some k
            else
              None
          | UncoalescedAccesses ->
            if Memory.is_global a then
              Some k
            else
              None
          | CountAccesses ->
            Some k
        )
      |> Variable.Set.of_list
    in*)
    let block_dim = lookup_dim3 "blockDim" Dim3.{x=32;y=1;z=1} env in
    let grid_dim = lookup_dim3 "gridDim" Dim3.{x=1;y=1;z=1} env in
    let config = Config.make ~block_dim ~grid_dim () in
    print_endline (Dim3.to_string block_dim);
    let linearize = Linearize_index.Default.linearize config k.arrays in
    let ctx =
      Vectorized.make ~bank_count ~thread_count:bank_count ~linearize
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


let main (fname : string) (m:Metric.t) : unit =
  try
    let parsed = Protocol_parser.Default.to_proto fname in
    let proto = parsed.kernels in
    let env = Env.load "env.json" in
    print_endline ("env.json: " ^ Env.to_string env);
    try
      List.iter (fun k ->
        print_endline (Kernel.to_string k);
        let ctx =
          Env.to_vectorized ~bank_count:32 ~env k
          |> Vectorized.restrict k.pre
        in
        let v = Vectorized.eval ~verbose:true m k.code ctx in
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

let metric =
  let doc = "Select the metric to measure the cost." in
  Arg.(required & opt (some (enum Metric.choices)) None & info ["m"; "metric"] ~doc)

let main_t = Term.(const main $ get_fname $ metric)

let info =
  let doc = "Dynamic performance analysis for GPU programs" in
  Cmd.info "faial-bc-dyn" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit
