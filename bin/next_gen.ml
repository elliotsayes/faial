module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap
module Environ = Z3_solver.Environ
module Witness = Z3_solver.Witness
module Vec3 = Z3_solver.Vec3
module Task = Z3_solver.Task
module T = ANSITerminal

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

let box_environ (e:Environ.t) : PrintBox.t =
  PrintBox.(
    v_record (List.map (fun (k,v) -> (k, text v)) e)
    |> frame
  )

let vec_to_s (v: Vec3.t) : string =
  "x = " ^ v.x ^ " │ y = " ^ v.y ^ " │ z = " ^ v.z

let box_tasks (t1:Task.t) (t2:Task.t) : PrintBox.t =
  let open PrintBox in
  let locals =
    t1.locals
    |> List.map (fun (k, v1) -> 
      [| text k; text v1; text (List.assoc k t2.locals) |]
    )
  in
  let locals = [| text "threadIdx"; text @@ vec_to_s t1.thread_idx; text @@ vec_to_s t2.thread_idx |] :: locals
  |> Array.of_list
in
  grid locals |> frame

let box_globals (w:Witness.t) : PrintBox.t =
  [
    "index", Common.join " │ " w.indices;
    "blockDim", vec_to_s w.block_dim;
    "blockIdx", vec_to_s w.block_idx;
    "gridDim", vec_to_s w.grid_dim;
  ] @
  w.globals
  |> box_environ

let box_locals (w:Witness.t) : PrintBox.t =
  let (t1, t2) = w.tasks in
  box_tasks t1 t2

let print_box: PrintBox.t -> unit =
  PrintBox_text.output stdout


let main (fname: string) : unit =
  let j = Cu_to_json.cu_to_json ~ignore_fail:true fname in
  let p = parse_imp j in
  List.iter (fun p ->
    let p = Imp.compile p in
    let kernel_name = p.kernel_name in
    let key_vals =
      Proto.kernel_constants p
      |> List.filter (fun (x,_) ->
        (* Make sure we only replace thread-global variables *)
        VarSet.mem (Exp.var_make x) p.kernel_global_variables
      )
    in
    let p = Proto.replace_constants key_vals p in
    let p = Wellformed.translate p in
    let p = Phasealign.translate p in
    let p = Phasesplit.translate p false in
    let p = Locsplit.translate p in
    let p = Flatacc.translate p in
    let p = Symbexp.translate true p in
    let open Z3_solver in
    let open Solution in
    let errors = solve p
      |> Streamutil.map_opt (
        function
        | Drf -> None
        | Unknown -> Some (Either.Left p)
        | Racy w -> Some (Either.Right (p, w))
      )
      |> Streamutil.to_list
    in
    match Common.either_split errors with
    | [], [] -> T.print_string [T.Bold; T.Foreground T.Green] ("Kernel '" ^ kernel_name ^ "' is DRF!\n")
    | _, l ->
      T.print_string [T.Bold; T.Foreground T.Red] ("Kernel '" ^ kernel_name ^ "' has errors.\n");
      let (_, l) = List.split l in
      l |> List.iter (fun w ->
        print_endline "Globals";
        box_globals w |> print_box;
        print_endline "\n\nLocals";
        box_locals w |> print_box;
        print_endline "";
      )
    | x :: _, _ -> print_endline "unknown!"
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

