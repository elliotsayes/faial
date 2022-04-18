(* Type-safe representation of a CType *)
type t = CType: string -> t

let make (ty:string) : t =
  CType ty

let to_string (c:t) : string =
  match c with
  | CType x -> x

let get_array_length (c:t) : int list =
  to_string c
  |> Common.parse_array_dim_opt
  |> Ojson.unwrap_or []

let get_array_type (c:t) : string list =
  to_string c
  |> Common.parse_array_type_opt
  |> Ojson.unwrap_or []

let is_array (c:t) : bool =
  to_string c
  |> Common.parse_array_type_opt
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
  ]

