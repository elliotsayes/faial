module StringMap = Common.StringMap
module VarSet = Exp.VarSet
module VarMap = Exp.VarMap
module Environ = Z3_solver.Environ
module Witness = Z3_solver.Witness
module Vec3 = Z3_solver.Vec3
module Task = Z3_solver.Task
module T = ANSITerminal

module Dim3 = struct
  type t = {x : int; y: int; z: int;}
  let mk ?(x=1) ?(y=1) ?(z=1) () : t = {x=x; y=y; z=z}

  let parse_opt (l:string) : t option =
    let len = String.length l in
    if len >= 2 && String.get l 0 = '[' && String.get l (len - 1) = ']' then
      let l = String.sub l 1 (len - 2) in
      match String.split_on_char ',' l |> List.map int_of_string with
      | [x; y; z] -> Some (mk ~x ~y ~z ())
      | [x; y] -> Some (mk ~x ~y ())
      | [x] -> Some (mk ~x ())
      | [] -> Some (mk ())
      | _ -> None
    else
      None
  let parse (l:string) : t =
    match parse_opt l with
    | Some x -> x
    | None -> failwith ("Dim3.parse: " ^ l)

  let to_string (d:t) : string =
    let x = string_of_int d.x in
    let y = string_of_int d.y in
    let z = string_of_int d.z in
    "{x=" ^ x ^ ", y=" ^ y ^ ", z=" ^ z ^ "}"
  let to_assoc (prefix:string) (d:t) : (string * int) list =
    [
      (prefix ^ "x", d.x);
      (prefix ^ "y", d.y);
      (prefix ^ "z", d.z)
    ] 
end

module GvParser = struct
  type t = {
    pass: bool;
    block_dim: Dim3.t;
    grid_dim: Dim3.t;
    options: string list;
  }

  let to_string (x:t) : string =
    "{pass=" ^ (if x.pass then "true" else "false") ^
      ", block_dim=" ^ Dim3.to_string x.block_dim ^
      ", grid_dim=" ^ Dim3.to_string x.grid_dim ^ "}"

  let to_assoc (x:t) : (string * int) list =
    Dim3.to_assoc "blockDim." x.block_dim @ Dim3.to_assoc "gridDim." x.grid_dim 

  let parse (filename:string) : t option =
    let header filename : string * string =
      let ic = open_in filename in
      let l1 = input_line ic in
      let l2 = input_line ic in
      close_in ic;
      (l1, l2)
    in
    let parse_line (l:string) : string list option =
      let len = String.length l in
      if len >= 2 && String.starts_with ~prefix:"//" l then
        String.sub l 2 (len - 2)
        |> String.trim
        |> String.split_on_char ' '
        |> Option.some
      else
        None
    in
    let parse_header (l1, l2: string * string) =
      let l1 = parse_line l1 in
      let l2 = parse_line l2 in
      match l1, l2 with
      | Some ["pass"], Some l2 -> Some (true, l2)
      | Some ["fail"], Some l2 -> Some (false, l2)
      | _, _ -> None
    in
    let parse_opts opts (l:string list) =
      let offset ~prefix:(sep:string) (part:string) : string option =
        if String.starts_with ~prefix:sep part then
          let len = String.length sep in
          Some (String.sub part len (String.length part - len))
        else None
      in
      List.fold_left (fun (x:t) (part:string) : t ->
        match offset ~prefix:"--blockDim=" part with
        | Some block_dim -> { x with block_dim = Dim3.parse block_dim }
        | None ->
          (match offset ~prefix:"--gridDim=" part with
            | Some grid_dim -> {x with grid_dim = Dim3.parse grid_dim }
            | None ->
              (match offset ~prefix:"-D" part with
                | Some opt -> {x with options = x.options @ [opt]}
                | None ->
                  prerr_endline ("GvParser.parse: skipping parameter: " ^ part);
                  x
              ))
      ) opts l
    in  
    let (l1, l2) = header filename in
    match parse_header (l1, l2) with
    | Some (pass, opts) ->
      Some (parse_opts {
        pass = pass;
        block_dim = Dim3.mk ();
        grid_dim = Dim3.mk ();
        options = [];
      } opts)
    | None -> None
end


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
  let gv = GvParser.parse fname in
  (match gv with
    | Some x -> prerr_endline ("WARNING: parsed GV args: " ^ GvParser.to_string x);
    | None -> ()
  );
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
    let key_vals = match gv with
    | Some gv -> GvParser.to_assoc gv @ key_vals
    | None -> key_vals
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
    | unk, errs ->
      T.print_string [T.Bold; T.Foreground T.Red] ("Kernel '" ^ kernel_name ^ "' has errors.\n");
      let (_, errs) = List.split errs in
      errs |> List.iter (fun w ->
        print_endline "Globals";
        box_globals w |> print_box;
        print_endline "\n\nLocals";
        box_locals w |> print_box;
        print_endline "";
        exit 1
      );
      print_endline "Unknown!"
  ) p

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let main_t = Term.(const main $ get_fname)

let info =
  let doc = "Print the C-AST" in
  Term.info "next-gen" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (main_t, info)

