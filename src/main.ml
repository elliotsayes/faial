open Proto
open Common
open Cmdliner
open Phasesplit

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

let sexp_parse input : prog kernel =
  let open Sexplib in
  let s : Sexp.t = Sexp.input_sexp input in
    safe_run (fun () -> Parse.parse_kernel.run s)

let json_parse ic =
  safe_run (fun () ->
    Yojson.Basic.from_channel ic
      |> Parsejs.parse_kernels.run
      |> List.map Program.compile
  )

let print_kernel k =
  let open Serialize in
  print_endline "; proto";
  kernel_ser k |> s_print

let print_kernel_P p =
  let open Serialize in
  print_endline "; phased";
  s_prog_ser p |> s_print

let print_kernel_C p =
  let open Serialize in
  print_endline "; conc";
  Stream.iter (fun x -> u_phase_ser x |> s_print)

let print_kernel_H p =
  let open Serialize in
  print_endline "; symbolic hist" (*;
  _phase_list_ser p |> s_print*)

type command = ALang | PLang | CLang | HLang | Sat | Typecheck

let print_errs errs =
  let open Typecheck in
  let print_err msg loc =
    Printf.printf "%a: %s" Sourceloc.location_print_start loc msg;
    try
      (Printf.printf "%a\n" Sourceloc.location_print_title loc)
    with Sys_error _ -> ()
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

  let halt_when b f k : unit =
    if b then (f k; raise Exit)
    else ()
  in

  let do_main cmd fname use_bv skip_po skip_drf use_json sets =
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
      List.iter (fun k ->
        Typecheck.typecheck_kernel k |> print_errs;
        let k = Subst.replace_constants sets k in
        halt_when (cmd = Typecheck) print_kernel k;
        let k = Phasesplit.prog_to_s_prog k.kernel_code in
        halt_when (cmd = ALang) print_kernel_P k;
        let k = Phasesplit.prog_to_phase_stream k in
        halt_when (cmd = CLang) print_kernel_C k
        (*
        let k = Phasesplit.project_phase_list k in
        halt_when (cmd = CLang) print_kernel_H k
        *)
      ) ks
    with
      | Exit -> ()
      | e ->
      print_endline "error!";
      close_in_noerr ic;
      raise e
  in
  let get_cmd =
    (* Override the codegeneration (for debugging only). *)
    let doc = "Step 0: Replace key-values and typecheck the kernel." in
    let tc = Typecheck, Arg.info ["0"; "check"] ~doc in
    let doc = "Step 1: Unroll loops, inline conditions" in
    let k1 = ALang, Arg.info ["1"; "a"] ~doc in
    let doc = "Step 2: Split instructions into phases" in
    let k2 = CLang, Arg.info ["2"; "c"] ~doc in
    let doc = "Step 3: Change loops into Variable Declarations" in
    let k3 = PLang, Arg.info ["3"; "p"] ~doc in
    let doc = "Step 4: Change loops into Variable Declarations" in
    let k4 = HLang, Arg.info ["4"; "h"] ~doc in
    let doc = "Step 5: generate Z3." in
    let sat = Sat, Arg.info ["5"; "sat"] ~doc in
    Arg.(last & vflag_all [Sat] [tc; k1; k2; k3; k4; sat])
  in
  Term.(const do_main $ get_cmd $ get_fname $ use_bv $ skip_po $ skip_drf $ use_json $ decls)

let info =
  let doc = "Verifies a GPU contract" in
  Term.info "main" ~doc ~exits:Term.default_exits



let _ =
  Term.exit @@ Term.eval (main_t, info)
