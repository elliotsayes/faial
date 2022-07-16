module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap


let parse_imp (j:Yojson.Basic.t) : Imp.p_kernel list =
  let open Indexflow in
  let open Cast in
  let open D_to_imp in
  match Cast.parse_program j with
  | Ok k1 ->
    let k2 = Dlang.rewrite_program k1 in
      (match D_to_imp.parse_program k2 with
      | Ok k3 -> k3
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
  let p = parse_imp j in
  List.iter (fun p ->
    let p = Imp.compile p in
    let p = Wellformed.translate p in
    let p = Phasealign.translate p in
    let p = Phasesplit.translate p false in
    let p = Locsplit.translate2 p in
    let p = Flatacc.translate2 p in
    let p = Symbexp.translate2 true p in
    Z3_solver.solve p
  ) p

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Print the C-AST" in
  Term.info "c-ast" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (main_t, info)

