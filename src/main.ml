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

(** Human-readable parser: *)
let v2_parse fname input : kernel =

  let filebuf = Lexing.from_channel input in
  Scan.set_filename filebuf fname;
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    let sloc = Sourceloc.of_lexbuf filebuf in
    Printf.fprintf stderr "%a: syntax error\n" Sourceloc.location_print_start sloc;
    Printf.fprintf stderr "\n%a" Sourceloc.location_print_title sloc;
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
  let print_err msg loc =
    Printf.printf "%a: %s\n%a"
      Sourceloc.location_print_start loc
      msg
      Sourceloc.location_print_title loc
  in
  let print_vars msg l =
    List.iter (fun x ->
      print_err (msg ^ " '" ^ x.var_name ^ "'\n") x.var_loc
    ) l
  in
  List.iter (fun x ->
    match x with
    | DuplicateLocs l -> print_vars "Duplicate location" l
    | DuplicateVars l -> print_vars "Duplicate variable" l
    | UndefinedLocs l -> print_vars "Undefined location" l
    | UndefinedVars l -> print_vars "Undefined variable" l
  )
  errs;
  if List.length errs > 0 then
    exit (-1)
  else ()

let main_t =
  let use_bv =
    let doc = "Generate bit-vector code." in
    Arg.(value & flag & info ["b"; "bv"] ~doc)
  in

  let skip_po =
    let doc = "Skip proof obligations." in
    Arg.(value & flag & info ["o"; "proof-oblig"] ~doc)
  in

  let skip_drf =
    let doc = "Skip DRF proof." in
    Arg.(value & flag & info ["d"; "drf"] ~doc)
  in

  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CONTRACT" ~doc)
  in
  let do_main cmd fname use_bv skip_po skip_drf =
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
        (* -- *)
        |> Smt.kernel_to_proofs (not skip_drf) (not skip_po)
        |> (if use_bv
           then Genfol.bv_serialize_proofs
           else Genfol.int_serialize_proofs)
        |> Genfol.print_code

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
  Term.(const do_main $ get_cmd $ get_fname $ use_bv $ skip_po $ skip_drf)

let info =
  let doc = "Verifies a GPU contract" in
  Term.info "main" ~doc ~exits:Term.default_exits



let _ =
  Term.exit @@ Term.eval (main_t, info)
