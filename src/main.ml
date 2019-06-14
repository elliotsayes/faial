open Proto
open Common
open Lexing
open Cmdliner

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
    Arg.(value & pos 0 string "/dev/stdin" & info [] ~docv:"CONTRACT" ~doc)
  in
  let do_main cmd fname use_bv =
    let ic = open_in fname in
    try
      let k = v2_parse ic in
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
