open Exp
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
let v2_parse fname input : prog kernel =

  let filebuf = Lexing.from_channel input in
  Scan.set_filename filebuf fname;
  try Parse2.main Scan.read filebuf with
  | Parse2.Error ->
    let sloc = Sourceloc.of_lexbuf filebuf in
    Printf.fprintf stderr "%a: syntax error\n" Sourceloc.location_print_start sloc;
    (try
      Printf.fprintf stderr "\n%a" Sourceloc.location_print_title sloc
    with
      Sys_error _ -> ());
    exit (-1)

(** Machine-readable parser: *)
let safe_run f =
  try
    f ()
  with
  | Parse.ParseError l ->
    List.iter (fun x ->
      print_endline x;
      print_endline ""
    ) l;
    exit (-1)

let json_parse ic =
  safe_run (fun () ->
    Yojson.Basic.from_channel ic
      |> Parsejs.parse_kernels.run
      |> List.map Imp.compile
  )


type command = ALang | PLang | CLang | HLang | BLang | Sat | Typecheck

let main_t =
  let use_bv =
    let doc = "Generate bit-vector code." in
    Arg.(value & flag & info ["b"; "bv"] ~doc)
  in

  let expect_typefail =
    let doc = "Expect typechecking failure." in
    Arg.(value & flag & info ["expect-invalid"] ~doc)
  in

  let use_json =
    let doc = "Parse a JSON file" in
    Arg.(value & flag & info ["json"] ~doc)
  in

  let decls =
    let doc = "Set the value of certain variables. Formatted as -D<variable>=<value>. For instance, '-DblockDim.x=512'" in
    Arg.(value & opt_all string [] & info ["D"; "set"] ~docv:"KEYVALS" ~doc)
  in

  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CONTRACT" ~doc)
  in

  let do_main cmd fname use_bv use_json expect_typing_fail sets =
    let halt_when b f k : unit =
      if b then (f k; raise Exit)
      else ()
    in

    let ic = open_in fname in
    let on_kv x =
      let kv = String.split_on_char '=' x in
      let kv = List.map String.trim kv in
      match kv with
      | [k; v] -> Some (k, int_of_string v)
      | _ -> None
    in
    let sets = List.map on_kv sets |> flatten_opt in
    try
      let ks = if use_json
        then json_parse ic
        else
          [v2_parse fname ic]
      in
      if List.length ks = 0 then begin
        print_endline "Error: kernel not found!";
        exit (1)
      end else
      List.iter (fun k ->
        if Typecheck.typecheck_kernel k |> Sourceloc.print_errs then
          exit (if expect_typing_fail then 0 else -1)
        else
          ()
        ;
        let k = Subst.replace_constants sets k in
        halt_when (cmd = Typecheck) Serialize.PPrint.print_k k;
        let ks = Phasealign.translate k in
        halt_when (cmd = ALang) Phasealign.print_kernels ks;
        let ks = Phasesplit.translate k expect_typing_fail in
        halt_when (cmd = PLang) Phasesplit.print_kernels ks;
        let ks = Locsplit.translate ks in
        halt_when (cmd = CLang) Locsplit.print_kernels ks;
        let ks = Flatacc.translate ks in
        halt_when (cmd = HLang) Flatacc.print_kernels ks;
        let ks = Symbexp.translate ks in
        halt_when (cmd = BLang) Symbexp.print_kernels ks;
        let ks = Gensmtlib2.translate ks in
        if expect_typing_fail then exit(-1)
        else Gensmtlib2.print ks
      ) ks
    with
      | Exit ->
        begin
          if expect_typing_fail then exit(-1)
          else ()
        end
      | e ->
      print_endline "error!";
      close_in_noerr ic;
      raise e
  in
  let get_cmd =
    (* Override the codegeneration (for debugging only). *)
    let doc = "Step 0: Replace key-values and typecheck the kernel." in
    let tc = Typecheck, Arg.info ["0"] ~doc in
    let doc = "Step 1: Align phases" in
    let k1 = ALang, Arg.info ["1"] ~doc in
    let doc = "Step 2: Split phases" in
    let k2 = PLang, Arg.info ["2"] ~doc in
    let doc = "Step 3: Split per location" in
    let k3 = CLang, Arg.info ["3"] ~doc in
    let doc = "Step 4: Flatten phases" in
    let k4 = HLang, Arg.info ["4"] ~doc in
    let doc = "Step 5: Generate booleans" in
    let k5 = BLang, Arg.info ["5"] ~doc in
    let doc = "Step 6: Generate SMT." in
    let sat = Sat, Arg.info ["6"; "sat"] ~doc in
    Arg.(last & vflag_all [Sat] [tc; k1; k2; k3; k4; k5; sat])
  in
  Term.(const do_main $ get_cmd $ get_fname $ use_bv $ use_json $ expect_typefail $ decls)

let info =
  let doc = "Verifies a GPU contract" in
  Term.info "faial-bin" ~doc ~exits:Term.default_exits



let _ =
  Term.exit @@ Term.eval (main_t, info)
