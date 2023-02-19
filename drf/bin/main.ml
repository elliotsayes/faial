open Stage0
open Protocols
open Inference
open Drf

module Environ = Z3_solver.Environ
module Witness = Z3_solver.Witness
module Vec3 = Z3_solver.Vec3
module Task = Z3_solver.Task
module T = ANSITerminal

let box_environ (e:Environ.t) : PrintBox.t =
  PrintBox.(
    v_record (List.map (fun (k,v) -> (k, text v)) e)
    |> frame
  )

let struct_to_s (l:(string * string) list) : string =
  l
  |> List.map (fun (key, elem) -> (key ^ " = " ^ elem))
  |> Common.join " | "

let dim_to_s (v: Vec3.t) : string =
  ["x", v.x; "y", v.y; "z", v.z]
  |> List.filter (fun (_, v) -> v <> "1")
  |> struct_to_s


let idx_to_s ~idx ~dim =
  let pos_fields =
    Vec3.to_assoc dim
    |> List.filter_map (fun (k, v) -> if v = "1" then None else Some k)
  in
  idx
  |> Vec3.to_assoc
  |> List.filter (fun (k, _) -> List.mem k pos_fields)
  |> struct_to_s


let box_idx key ~idx ~dim =
  let idx = idx_to_s ~idx ~dim in
  if idx = "" then []
  else [key, idx]


let box_tasks (block_dim:Vec3.t) (t1:Task.t) (t2:Task.t) : PrintBox.t =
  let open PrintBox in
  let locals =
    t1.locals
    |> List.map (fun (k, v1) -> 
      [| text k; text v1; text (List.assoc_opt k t2.locals |> Ojson.unwrap_or "?") |]
    )
  in
  let locals =
    [|
      text "threadIdx";
      text @@ idx_to_s ~idx:t1.thread_idx ~dim:block_dim;
      text @@ idx_to_s ~idx:t2.thread_idx ~dim:block_dim;
    |] :: locals
    |> Array.of_list
  in
  grid locals |> frame

let box_globals (w:Witness.t) : PrintBox.t =
  let dim x = if x = "1" then 0 else 1 in
  let dim_len v =
    let open Vec3 in
    dim v.x + dim v.y + dim v.z
  in
  let box_dim name v =
    if dim_len v = 0 then []
    else
      [name, dim_to_s v]
  in
  [
    "index", Common.join " │ " w.indices;
  ]
  @ box_dim "gridDim" w.grid_dim
  @ box_idx "blockIdx" ~idx:w.block_idx ~dim:w.grid_dim
  @ box_dim "blockDim" w.block_dim
  @ w.globals
  |> box_environ

let box_locals (w:Witness.t) : PrintBox.t =
  let (t1, t2) = w.tasks in
  box_tasks w.block_dim t1 t2

let print_box: PrintBox.t -> unit =
  PrintBox_text.output stdout

let main
  (fname: string)
  (timeout:int option)
  (show_proofs:bool)
  (show_proto:bool)
  (show_wf:bool)
  (show_align:bool)
  (show_phase_split:bool)
  (show_loc_split:bool)
  (show_flat_acc:bool)
  (show_symbexp:bool)
  (logic:string option)
:
  unit
=
  let parsed = Protocol_parser.Silent.to_proto fname in
  let grid_dim = parsed.options.grid_dim |> Vec3.from_dim3 in
  let block_dim = parsed.options.block_dim |> Vec3.from_dim3 in
  parsed.kernels
  |> List.iter (fun (p:Proto.prog Proto.kernel) ->
    let kernel_name = p.kernel_name in
    let show (b:bool) (call:unit -> unit) : unit =
      if b then call () else ()
    in
    show show_proto (fun () -> Proto.print_k p);
    let p = Wellformed.translate p in
    show show_wf (fun () -> Wellformed.print_kernels p);
    let p = Phasealign.translate p in
    show show_align (fun () -> Phasealign.print_kernels p);
    let p = Phasesplit.translate p false in
    show show_phase_split (fun () -> Phasesplit.print_kernels p);
    let p = Locsplit.translate p in
    show show_loc_split (fun () -> Locsplit.print_kernels p);
    let p = Flatacc.translate p in
    show show_flat_acc (fun () -> Flatacc.print_kernels p);
    let p = Symbexp.translate p in
    show show_symbexp (fun () -> Symbexp.print_kernels p);
    let open Z3_solver in
    let open Solution in
    let errors =
      p
      |> solve
          ~grid_dim:(Some grid_dim)
          ~block_dim:(Some block_dim)
          ~timeout:timeout
          ~show_proofs
          ~logic
      |> Streamutil.map_opt (
        function
        | Drf -> None
        | Unknown -> Some (Either.Left p)
        | Racy w -> Some (Either.Right (p, w))
      )
      |> Streamutil.to_list
    in
    let print_errors errs =
      errs |> List.iteri (fun i (w:Witness.t) ->
        T.print_string [T.Bold; T.Foreground T.Blue] ("\n~~~~ Data-race " ^ string_of_int (i + 1) ^ " ~~~~\n\n");
        let locs =
          let (t1, t2) = w.tasks in
          let l = [t1.location; t2.location] |> Common.flatten_opt in
          match l with
          | [x1; x2] when x1 = x2 -> [x1]
          | [x1; x2] when x2 < x1 -> [x2; x1]
          | _ -> l
        in
        (match locs with
        | [x] -> Tui.LocationUI.print x
        | [x1; x2] -> Tui.LocationUI.print2 x1 x2
        | _ -> failwith "??"
        );
        print_endline "";
        T.print_string [T.Bold] ("Globals\n");
        box_globals w |> print_box;
        T.print_string [T.Bold] ("\n\nLocals\n");
        box_locals w |> print_box;
        print_endline "";
        T.print_string [T.Underlined] ("(proof #" ^ string_of_int w.proof_id ^ ")\n");
      );
    in
    match Common.either_split errors with
    | [], [] -> T.print_string [T.Bold; T.Foreground T.Green] ("Kernel '" ^ kernel_name ^ "' is DRF!\n")
    | unk, errs ->
      let has_unknown = List.length unk > 0 in
      let errs = List.split errs |> snd in
      let err_count = List.length errs |> string_of_int in
      let dr = "data-race" ^ (if err_count = "1" then "" else "s") in
      T.print_string [T.Bold; T.Foreground T.Red] ("Kernel '" ^ kernel_name ^ "' has " ^ err_count ^ " " ^ dr ^ ".\n");
      print_errors errs;
      if has_unknown then
        T.print_string [T.Foreground T.Red] ("A portion of the kernel was not analyzable. Try to increasing the timeout.\n")
      else ();
      if err_count <> "0" || has_unknown then
        exit 1
      else
        ()
  )

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let get_timeout =
  let doc = "Sets a timeout in millisecs. Default: $(docv)" in
  Arg.(value & opt (some int) None & info ["t"; "timeout"] ~docv:"MILISECS" ~doc)

let logic =
  let doc = "Set the logic used by the Z3 solver." in
  Arg.(value & opt (some string) None & info ["logic"] ~doc)

let show_proofs =
  let doc = "Show the Z3 proofs being generated." in
  Arg.(value & flag & info ["show-proofs"] ~doc)

let show_proto =
  let doc = "Show the MAP kernel." in
  Arg.(value & flag & info ["show-map"] ~doc)

let show_wf =
  let doc = "Show the well-formed kernel." in
  Arg.(value & flag & info ["show-well-formed"] ~doc)

let show_aligned =
  let doc = "Show the aligned kernel." in
  Arg.(value & flag & info ["show-aligned"] ~doc)

let show_phase_split =
  let doc = "Show the phase-split kernel." in
  Arg.(value & flag & info ["show-phase-split"] ~doc)

let show_loc_split =
  let doc = "Show the location-split kernel." in
  Arg.(value & flag & info ["show-loc-split"] ~doc)

let show_flat_acc =
  let doc = "Show the flat-access kernel." in
  Arg.(value & flag & info ["show-flat-acc"] ~doc)

let show_symbexp =
  let doc = "Show the symbexp kernel." in
  Arg.(value & flag & info ["show-symbexp"] ~doc)

let main_t = Term.(
  const main
  $ get_fname
  $ get_timeout
  $ show_proofs
  $ show_proto
  $ show_wf
  $ show_aligned
  $ show_phase_split
  $ show_loc_split
  $ show_flat_acc
  $ show_symbexp
  $ logic
)

let info =
  let doc = "Print the C-AST" in
  Cmd.info "faial-drf" ~version:"%%VERSION%%" ~doc

let () =
  Cmd.v info main_t
  |> Cmd.eval
  |> exit

