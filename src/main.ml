open Proto
open Common
open Cmdliner
(** Prints the nth-line of a file (starts at base 0) *)
let nth_line filename n =
  let ic = open_in filename in
  let rec iter i =
    let line = input_line ic in
    if i = n then begin
      close_in ic;
      (line)
    end else iter (succ i)
  in
  iter 0

let underline offset count : string =
  (String.make offset ' ') ^ (String.make count '^')


(** Human-readable parser: *)
let v2_parse fname input : kernel =

  let open Lexing in
  (* Return the line number and position of a position *)
  let get_file_offset pos = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1) in
  (* Prints the file offset *)
  let print_position outx lexbuf =
    let lineno, offset = get_file_offset lexbuf.lex_start_p in
    Printf.fprintf outx "%s:%d:%d" fname lineno offset
  in
  (* Prints the line number in the given source location *)
  let print_data outx lexbuf =
    let start_line, start_off = get_file_offset lexbuf.lex_start_p in
    let start_idx = start_off - 1 in
    let err_text = nth_line fname (start_line - 1) in
    let end_line, end_off = get_file_offset lexbuf.lex_curr_p in
    let end_idx = end_off - 1 in
    let count =
      if start_line != end_line
      then String.length err_text
      else end_off - start_off
    in
    (*
    Printf.fprintf outx "%s\n%s" err_text (underline start_off end_off)
    *)
    Printf.fprintf outx "%s\n" err_text;
    Printf.fprintf outx "%s\n" (underline start_idx count);

  in
  let filebuf = from_channel input in
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position filebuf;
    Printf.fprintf stderr "\n%a" print_data filebuf;
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

let print_flat_kernel k =
  Serialize.flat_kernel_ser k
    |> Sexplib.Sexp.to_string_hum
    |> print_endline

let print_proj_kernel t =
  Serialize.proj_ser t
    |> Sexplib.Sexp.to_string_hum
    |> print_endline

type command = Flatten | Project | Sat

let print_errs errs =
  let open Typecheck in
  List.iter (fun x ->
    (match x with
    | DuplicateLocs l -> "Duplicate locations: " ^ (join ", " l)
    | DuplicateVars l -> "Duplicate variables: " ^ (join ", " l)
    | UndefinedLocs l -> "Undefined locations: " ^ (join ", " l)
    | UndefinedVars l -> "Undefined variables: " ^ (join ", " l)
    ) |> print_endline
  )
  errs;
  if List.length errs > 0 then
    exit (-1)
  else ()

let main_t =
  let use_bv =
    let doc = "Generate bit-vector code." in
    Arg.(value & flag & info ["b"; "generate-bv"] ~doc)
  in

  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CONTRACT" ~doc)
  in
  let do_main cmd fname use_bv =
    let ic = open_in fname in
    try
      let k = v2_parse fname ic in
      Typecheck.typecheck_kernel k |> print_errs;
      match cmd with
      | Flatten -> Loops.flatten_kernel k
        |> print_flat_kernel
      | Project ->
        Loops.flatten_kernel k
        |> Spmd2binary.project_kernel
        |> print_proj_kernel
      | Sat ->
        Loops.flatten_kernel k
        |> Spmd2binary.project_kernel
        |> if use_bv
            then Genfol.Bv.iter_generated_code
            else Genfol.Std.iter_generated_code
    with e ->
      close_in_noerr ic;
      raise e
  in
  let get_cmd =
    (* Override the codegeneration (for debugging only). *)
    let doc = "Step 1: remove loops and merge assertions." in
    let flat = Flatten, Arg.info ["1"; "flat"] ~doc in
    let doc = "Step 2: project into two tasks." in
    let proj = Project, Arg.info ["2"; "proj"] ~doc in
    let doc = "Step 3: generate Z3." in
    let sat = Sat, Arg.info ["3"; "sat"] ~doc in
    Arg.(last & vflag_all [Sat] [flat; proj; sat])
  in
  Term.(const do_main $ get_cmd $ get_fname $ use_bv)

let info =
  let doc = "Verifies a GPU contract" in
  Term.info "main" ~doc ~exits:Term.default_exits



let _ =
  Term.exit @@ Term.eval (main_t, info)
