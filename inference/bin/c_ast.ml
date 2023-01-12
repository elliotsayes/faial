open Stage0
open Inference
open Queries
module Decl = C_lang.Decl
let analyze (j:Yojson.Basic.t) : C_lang.c_program  * D_lang.d_program * (Imp.p_kernel list) =
  match C_lang.parse_program j with
  | Ok k1 ->
    let k2 = D_lang.rewrite_program k1 in
      (match D_to_imp.Default.parse_program k2 with
      | Ok k3 -> (k1, k2, k3)
      | Error e ->
        C_lang.print_program k1;
        print_endline "------";
        D_lang.print_program k2;
        print_endline "-------";
        D_to_imp.print_error e;
        exit(-1)
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)


let main
  (fname: string)
  (silent:bool)
: unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let (k1, k2, k3) = analyze j in
  if silent then () else ( 
    print_endline "\n==================== STAGE 1: C\n";
    C_lang.print_program k1;
    print_endline "==================== STAGE 2: C with reads/writes as statements\n";
    D_lang.print_program k2;
    print_endline "==================== STAGE 3: Memory access protocols\n";
    List.iter Imp.print_kernel k3;
    print_endline "==================== STAGE 4: stats\n";
  );
  let k1_len = List.length k1 in
  let k2_ht = Hashtbl.create k1_len in
  let k3_ht = Hashtbl.create k1_len in
  k2 |> List.iter (
    let open D_lang in
    function
    | Kernel k ->
      Hashtbl.add k2_ht k.name k
    | Declaration _ -> ()
  );
  k3 |> List.iter (fun k ->
    let open Imp in
    Hashtbl.add k3_ht k.p_kernel_name k
  );
  let l = List.fold_left (fun ((decls:Decl.t list), js) ->
    let open C_lang in
    function
    | Kernel k ->
      let open Imp in
      let k2 = Hashtbl.find k2_ht k.name in
      let k3 = Hashtbl.find k3_ht k.name in
      (decls, `Assoc [
        "name", `String k.name;
        "function calls", Calls.summarize decls k;
        "nested loops", NestedLoops.summarize k.code;
        "loops", Loops.summarize k.code;
        "loop inference", ForEach.summarize k2.code;
        "mutated vars", MutatedVar.summarize k.code;
        "declarations", Declarations.summarize k.code;
        "conditionals", Conditionals.summarize k.code;
        "variables", Variables.summarize k.code;
        "params", Params.summarize k;
        "accesses", Accesses.summarize k3.p_kernel_code;
        "global decls", GlobalDeclArrays.summarize decls;
        "divergence", Divergence.summarize k3.p_kernel_code;
      ] :: js)
    | Declaration d ->
      let decls =
        if Decl.is_array d then
          d::decls
        else
          decls
      in
      (decls, js)
  ) ([], []) k1 |> snd
  in
  print_endline (Yojson.Basic.pretty_to_string (`List l));

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let silent =
  let doc = "Silence output" in
  Arg.(value & flag & info ["silent"] ~doc)

let main_t = Term.(const main $ get_fname $ silent)

let info =
  let doc = "Print the C-AST" in
  Cmd.info "c-ast" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

