open Proto
open Cmdliner

(* ----------------- kernel functions -------------------- *)

let open_ic_with (fname:string option) (f : in_channel -> unit) : unit =
    let ic, (closer: in_channel -> unit) = match fname with
    | Some fname -> (open_in fname, close_in_noerr)
    | None -> (stdin, fun x -> ())
    in
    try (f ic; closer ic) with
    | e -> closer ic;
      raise e

let p_kernel_parser fname input : prog kernel =
  let fname = match fname with
  | Some x -> x
  | None -> "<STDIN>"
  in
  let filebuf = Lexing.from_channel input in
  Scan.set_filename filebuf fname;
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    let b = Buffer.create 1024 in
    let sloc = Sourceloc.of_lexbuf filebuf in
    Printf.bprintf b "%a: syntax error" Sourceloc.location_bprint_start sloc;
    (try
        Printf.bprintf b "%a" Sourceloc.location_bprint_title sloc
    with
        Sys_error _ -> ()
    );
    raise (Common.ParseError b)

let j_kernel_parser (_:string option) ic =
  try Yojson.Basic.from_channel ic |> Parsejs.parse_kernels.run with
  | Yojson.Json_error("Blank input data") ->
    (* If the input is blank, just ignore the input and err with -1 *)
    raise (Common.mk_parse_error_s "Empty input data. Blank file?\n")
  | Yojson.Json_error(e) ->
    raise (Common.mk_parse_error_s (Printf.sprintf "Error parsing JSON: %s\n" e))

type i_kernel =
  | JKernel of Imp.p_kernel list
  | PKernel of prog kernel

let parse_i_kernel (use_json:bool) (fname:string option) (ic:in_channel) : i_kernel =
  if use_json then
    JKernel (j_kernel_parser fname ic)
  else
    PKernel (p_kernel_parser fname ic)

let open_i_kernel_with (use_json:bool) (fname:string option) (f:i_kernel -> unit) : unit =
  open_ic_with fname (fun ic ->
    f (parse_i_kernel use_json fname ic)
  )

let i_kernel_to_p_kernel (k:i_kernel) : prog kernel list =
  match k with
  | JKernel ks -> List.map Imp.compile ks
  | PKernel p -> [p]

(* ----------------- command line argument parser -------------------- *)

let main_t =
  let use_json =
    let doc = "Parse a JSON file" in
    Arg.(value & flag & info ["json"] ~doc)
  in

  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"CONTRACT" ~doc)
  in

  let do_main
    (fname: string option)
    (use_json: bool)
    : unit =
  let print_cuda (k : i_kernel) : unit =
    List.iter (fun k -> Cgen.print_k k) (i_kernel_to_p_kernel k)
  in
  try open_i_kernel_with use_json fname print_cuda with
  | Common.ParseError b ->
      Buffer.output_buffer stderr b;
      exit (-1) in
  Term.(
    const do_main
    $ get_fname
    $ use_json
  )

let info =
  let doc = "Generates a CUDA file from a protocol" in
  Term.info "proto-to-cuda" ~doc ~exits:Term.default_exits

(* ----------------- execution entry point -------------------- *)

let _ =
  Term.eval (main_t, info)
  |> Term.exit
