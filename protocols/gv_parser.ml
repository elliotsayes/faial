type t = {
  pass: bool;
  block_dim: Dim3.t;
  grid_dim: Dim3.t;
  options: string list;
}

let default_grid_dim : Dim3.t = Dim3.{x=1; y=1; z=1}
let default_block_dim : Dim3.t = Dim3.{x=1024; y=1; z=1}

let default = {
  pass=true;
  block_dim=default_block_dim;
  grid_dim=default_grid_dim;
  options=[]
}

let to_string (x:t) : string =
  "{pass=" ^ (if x.pass then "true" else "false") ^
    ", block_dim=" ^ Dim3.to_string x.block_dim ^
    ", grid_dim=" ^ Dim3.to_string x.grid_dim ^ ", options=[" ^ String.concat ", " x.options ^ "]}"

let to_assoc (x:t) : (string * int) list =
  Dim3.to_assoc ~prefix:"blockDim." x.block_dim @ Dim3.to_assoc ~prefix:"gridDim." x.grid_dim

let parse (filename:string)
:
  t option
=
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
      | Some block_dim -> { x with block_dim = Dim3.parse block_dim |> Result.get_ok}
      | None ->
        (match offset ~prefix:"--gridDim=" part with
          | Some grid_dim -> {x with grid_dim = Dim3.parse grid_dim |> Result.get_ok }
          | None ->
            (match offset ~prefix:"-D" part with
              | Some opt -> {x with options = x.options @ [opt]}
              | None ->
                prerr_endline ("Gv_parser.parse: skipping parameter: " ^ part);
                x
            ))
    ) opts l
  in
  let (l1, l2) = header filename in
  match parse_header (l1, l2) with
  | Some (pass, opts) ->
    Some (parse_opts {
      pass = pass;
      block_dim = default_block_dim;
      grid_dim = default_grid_dim;
      options = [];
    } opts)
  | None -> None
