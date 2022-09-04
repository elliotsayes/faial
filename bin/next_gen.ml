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

  let to_vec3 (d:t) : Vec3.t =
    let x = string_of_int d.x in
    let y = string_of_int d.y in
    let z = string_of_int d.z in
    Vec3.mk ~x ~y ~z

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

let struct_to_s (l:(string * string) list) : string =
  l
  |> List.map (fun (key, elem) -> (key ^ " = " ^ elem))
  |> Common.join " | "

let vec_to_s (v: Vec3.t) : string =
  ["x", v.x; "y", v.y; "z", v.z]
  |> struct_to_s

let dim_to_s (v: Vec3.t) : string =
  ["x", v.x; "y", v.y; "z", v.z]
  |> List.filter (fun (_, v) -> v <> "1")
  |> struct_to_s


let idx_to_s ~idx ~dim =
  let pos_fields =
    Vec3.to_assoc dim
    |> Common.map_opt (fun (k, v) -> if v = "1" then None else Some k)
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


let main (fname: string) (timeout:int) : unit =
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
    let dim (f:GvParser.t -> Dim3.t) : Vec3.t option = match gv with
      | Some gv -> Some (Dim3.to_vec3 (f gv))
      | None -> None
    in
    let grid_dim = dim (fun gv -> gv.grid_dim) in
    let block_dim = dim (fun gv -> gv.block_dim) in
    let p = Proto.replace_constants key_vals p in
    let p = Wellformed.translate p in
    let p = Phasealign.translate p in
    let p = Phasesplit.translate p false in
    let p = Locsplit.translate p in
    let p = Flatacc.translate p in
    let p = Symbexp.translate true p in
    let open Z3_solver in
    let open Solution in
    let errors = solve ~grid_dim ~block_dim ~timeout:timeout p
      |> Streamutil.map_opt (
        function
        | Drf -> None
        | Unknown -> Some (Either.Left p)
        | Racy w -> Some (Either.Right (p, w))
      )
      |> Streamutil.to_list
    in
    let print_errors errs =
      errs |> List.iteri (fun i err ->
        T.print_string [T.Bold; T.Foreground T.Blue] ("\n~~~~ Data-race " ^ string_of_int (i + 1) ^ " ~~~~\n\n");
        T.print_string [T.Bold] ("Globals\n");
        box_globals err |> print_box;
        T.print_string [T.Bold] ("\n\nLocals\n");
        box_locals err |> print_box;
        print_endline "";
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
  ) p

open Cmdliner

let get_fname = 
  let doc = "The path $(docv) of the GPU program." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let get_timeout =
  let doc = "Sets a timeout in millisecs. Default: $(docv)" in
  Arg.(value & opt int 1000 & info ["t"; "timeout"] ~docv:"MILISECS" ~doc)

let main_t = Term.(const main $ get_fname $ get_timeout)

let info =
  let doc = "Print the C-AST" in
  Term.info "next-gen" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (main_t, info)

