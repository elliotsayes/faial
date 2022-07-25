module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap


let analyze (j:Yojson.Basic.t) : Cast.c_program  * Dlang.d_program * (Imp.p_kernel list) =
  let open Indexflow in
  let open Cast in
  let open D_to_imp in
  match Cast.parse_program ~include_auxiliary:true j with
  | Ok k1 ->
    let k2 = Dlang.rewrite_program k1 in
      (match D_to_imp.parse_program k2 with
      | Ok k3 -> (k1, k2, k3)
      | Error e ->
        Cast.print_program k1;
        print_endline "------";
        Dlang.print_program k2;
        print_endline "-------";
        D_to_imp.print_error e;
        exit(-1)
      )

  | Error e ->
    Rjson.print_error e;
    exit(-1)


let main (fname: string) : unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let (k1, k2, k3) = analyze j in 
  print_endline "\n==================== STAGE 1: C\n";
  Cast.print_program ~modifier:false k1;
  print_endline "==================== STAGE 2: C with reads/writes as statements\n";
  Dlang.print_program k2;
  print_endline "==================== STAGE 3: Memory access protocols\n";
  List.iter Imp.print_kernel k3

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Print the C-AST" in
  Term.info "c-ast" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (main_t, info)

