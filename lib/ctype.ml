
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
  to_string c |> Common.ends_with " *"

let get_array_length (c:t) : int list =
  to_string c
  |> parse_array_dim_opt
  |> Ojson.unwrap_or []

let get_array_type (c:t) : string list =
  to_string c
  |> parse_array_type_opt
  |> Ojson.unwrap_or []

let is_array (c:t) : bool =
  to_string c
  |> parse_array_type_opt
  |> Option.is_some

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
  ]

(* ------------------------------------- *)

let mk_j_type name =
  let open Yojson in
  `Assoc[
    "qualType", `String name
  ]

let j_int_type = mk_j_type "int"
let j_char_type = mk_j_type "char"
let j_bool_type = mk_j_type "bool"
let j_float_type = mk_j_type "float"
