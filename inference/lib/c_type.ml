open Stage0

let split_array_type (x:string) : (string * string) option =
  match Common.split '[' x with
  | Some (x, y) ->
    let x = String.trim x in
    Some (x, "[" ^ y)
  | None -> (
    match Common.rsplit ' ' x with
    | Some (_, "*") as o -> o
    | _ -> None
  )

let parse_dim (x:string) : int list =
  (*
    let ex3 = "[8][8]" in
    assert (parse_dim ex3 = Some [8; 8]);
  *)
  String.split_on_char '[' x
  |> List.concat_map (fun (x:string) ->
      if String.length x = 0 then []
      else [String.sub x 0 (String.length x - 1)]
    )
  |>
  List.map int_of_string

let parse_array_type_opt (x:string) : string list option =
  match split_array_type x with
  | Some (x, _) -> Some (
      String.split_on_char ' ' x
      |> List.filter (fun x -> String.length x > 0)
    )
  | None -> None

let parse_array_dim_opt (x:string) : int list option =
  match split_array_type x with
  | Some (_, x) -> (
      let x  = match Common.rsplit ' ' x with
      | Some (x, "*") -> x
      | _ -> x
      in
      (try Some (parse_dim x) with
        Failure _ -> None)
    )
  | None -> None

(* ----------------------------------- *)

(* Type-safe representation of a CType *)
type t = CType: string -> t

let make (ty:string) : t =
  CType ty

let to_string (c:t) : string =
  match c with
  | CType x -> x

let is_pointer (c:t) =
  to_string c |> String.ends_with ~suffix:" *"

let get_array_length (c:t) : int list =
  to_string c
  |> parse_array_dim_opt
  |> Option.value ~default:[]

let get_array_type (c:t) : string list =
  to_string c
  |> parse_array_type_opt
  |> Option.value ~default:[]

let is_array (c:t) : bool =
  to_string c
  |> parse_array_type_opt
  |> Option.is_some

let sizeof (x:t) : int option =
  let x = to_string x in
  if String.ends_with ~suffix:"*" x then Some 8
  else
  let x =
    x
    |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "const" || x <> "unsigned")
    |> Common.join " "
  in
  if String.starts_with ~prefix:"long" x then Some 8
  else if x = "int" then Some 4
  else if x = "short" then Some 2
  else if x = "char" then Some 1
  else if x = "float" then Some 4
  else if x = "double" then Some 8
  else None


let is_int (c:t) : bool =
  List.mem (to_string c) [
    "int";
    "const int";
    "unsigned int";
    "const unsigned int";
    "short";
    "const short";
    "uint";
    "const uint";
    "long";
    "const long";
    "bool";
    "const bool";
    "size_t";
    "const size_t";
  ]

(* ------------------------------------- *)

let mk_j_type name =
  `Assoc[
    "qualType", `String name
  ]

let j_int_type = mk_j_type "int"
let j_char_type = mk_j_type "char"
let j_bool_type = mk_j_type "bool"
let j_float_type = mk_j_type "float"

type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

let from_json (j:Yojson.Basic.t) : t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* ty = with_field "qualType" cast_string o in
  Ok (make ty)

let j_to_string (j:Yojson.Basic.t) : string =
  match from_json j with
  | Ok ty -> to_string ty
  | Error _ -> "?"

