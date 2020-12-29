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
    Printf.eprintf "%a: syntax error\n" Sourceloc.location_print_start sloc;
    (try
      Printf.eprintf "\n%a" Sourceloc.location_print_title sloc
    with
      Sys_error _ -> ());
    exit (-1)

(** Machine-readable parser: *)
let safe_run f =
  try
    f ()
  with
  | Yojson.Json_error("Blank input data") ->
    (* If the input is blank, just ignore the input and err with -1 *)
    exit (-1)
  | Yojson.Json_error(e) -> begin
      Printf.eprintf "Error parsing JSON: %s" e;
      exit (-1)
    end
  | Common.ParseError l ->
    List.iter (fun x ->
      prerr_endline x;
      prerr_endline ""
    ) l;
    exit (-1)

type command = ALang | PLang | CLang | HLang | BLang | Sat | Typecheck | Parse

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

  let skip_typecheck =
    let doc = "Skip the typechecking stage" in
    Arg.(value & flag & info ["skip-type-check"] ~doc)
  in

  let decls =
    let doc = "Set the value of certain variables. Formatted as -D<variable>=<value>. For instance, '-DblockDim.x=512'" in
    Arg.(value & opt_all string [] & info ["D"; "set"] ~docv:"KEYVALS" ~doc)
  in

  let get_fname =
    let doc = "The path $(docv) of the GPU contract." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"CONTRACT" ~doc)
  in

  let do_main
    (cmd: command)
    (fname: string option)
    (use_bv: bool)
    (use_json: bool)
    (expect_typing_fail: bool)
    (skip_typecheck: bool)
    (sets: string list) : unit
  =
    let halt_when b f k : unit =
      if b then (f k; raise Exit)
      else ()
    in

    let ic = match fname with
      | Some fname -> open_in fname
      | None -> stdin
    in
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
        then begin
          safe_run (fun () ->
            let ks = Yojson.Basic.from_channel ic
              |> Parsejs.parse_kernels.run
            in
            if cmd = Parse then
              (List.iter Imp.print_kernel ks;
              (* return an empty list of compiled kernels *)
              [])
            else
              List.map Imp.compile ks
          )
        end else
          let fname = match fname with
          | Some x -> x
          | None -> "<STDIN>"
          in
          [v2_parse fname ic]
      in
      if List.length ks = 0 && cmd != Parse then begin
        print_endline "Error: kernel not found!";
        exit (1)
      end else
      List.iter (fun k ->
        if not skip_typecheck && Typecheck.typecheck_kernel k |> Sourceloc.print_errs then
          exit (if expect_typing_fail then 0 else -1)
        else
          ()
        ;
        try
          let provenance = false in
          let key_vals =
            sets @ Proto.kernel_constants k
            |> List.filter (fun (x,_) ->
              (* Make sure we only replace thread-global variables *)
              VarSet.mem (var_make x) k.kernel_global_variables
            )
          in
          let k = Proto.replace_constants key_vals k in
          halt_when (cmd = Typecheck) Proto.print_k k;
          let ks = Phasealign.translate2 k in
          halt_when (cmd = ALang) Phasealign.print_kernel ks;
          let ks = Phasesplit.translate2 ks expect_typing_fail in
          halt_when (cmd = PLang) Phasesplit.print_kernels2 ks;
          let ks = Locsplit.translate2 ks in
          halt_when (cmd = CLang) Locsplit.print_kernels2 ks;
          let ks = Flatacc.translate2 ks in
          halt_when (cmd = HLang) Flatacc.print_kernels2 ks;
          let ks = Symbexp.translate2 provenance ks in
          halt_when (cmd = BLang) Symbexp.print_kernels ks;
          let ks = Gensmtlib2.translate provenance ks in
          Gensmtlib2.print ks;
          ()
        with Exit -> ()
      ) ks
    with
      | Phasesplit.PhasesplitException errs ->
          let _ = Sourceloc.print_errs errs in
          exit (if expect_typing_fail then 0 else -1)
      | e ->
      (match fname with
      | Some _ -> close_in_noerr ic
      | None -> ()
      );
      raise e
  in
  let get_cmd =
    (* Override the codegeneration (for debugging only). *)
    let doc = "Step 0: Print what was parsed" in
    let p = Parse, Arg.info ["0"] ~doc in
    let doc = "Step 1: Replace key-values and typecheck the kernel." in
    let tc = Typecheck, Arg.info ["1"] ~doc in
    let doc = "Step 2: Align phases" in
    let k1 = ALang, Arg.info ["2"] ~doc in
    let doc = "Step 3: Split phases" in
    let k2 = PLang, Arg.info ["3"] ~doc in
    let doc = "Step 4: Split per location" in
    let k3 = CLang, Arg.info ["4"] ~doc in
    let doc = "Step 5: Flatten phases" in
    let k4 = HLang, Arg.info ["5"] ~doc in
    let doc = "Step 6: Generate booleans" in
    let k5 = BLang, Arg.info ["6"] ~doc in
    let doc = "Step 7: Generate SMT." in
    let sat = Sat, Arg.info ["7"; "sat"] ~doc in
    Arg.(last & vflag_all [Sat] [p; tc; k1; k2; k3; k4; k5; sat])
  in
  Term.(
    const do_main
    $ get_cmd
    $ get_fname
    $ use_bv
    $ use_json
    $ expect_typefail
    $ skip_typecheck
    $ decls
  )

let info =
  let doc = "Verifies a GPU contract" in
  Term.info "faial-bin" ~doc ~exits:Term.default_exits



let _ =
  Term.exit @@ Term.eval (main_t, info)
