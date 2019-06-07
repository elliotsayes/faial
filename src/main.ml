open Proto
open Common
open Lexing

(** Human-readable parser: *)
let v2_parse input : kernel =
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  let filebuf = Lexing.from_channel input in
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position filebuf;
    exit (-1)

(** Machine-readable parser: *)
let sexp_parse input : kernel =
  let open Sexplib in
  let s : Sexp.t = Sexp.input_sexp input in
    try
      Parse.parse_kernel.run s
    with
    | Parse.ParseError l ->
      List.iter (fun x ->
        print_endline x
      ) l;
      exit (-1)

let main () =
  let open Sexplib in
  v2_parse stdin
  |> Loops.flatten_kernel
  |> Spmd2binary.project_kernel
  |> Genfol.iter_generated_code

let _ = main ()
