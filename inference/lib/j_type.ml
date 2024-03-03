open Stage0
open Protocols
(*
 Represents a c-to-json serialization of a data-type.
 *)

type t = Yojson.Basic.t

let from_json (j:Yojson.Basic.t) : t = j

let from_name (name:string) : t =
  `Assoc[
    "qualType", `String name
  ]

let int = from_name "int"
let char = from_name "char"
let bool = from_name "bool"
let float = from_name "float"
let void = from_name "void"
let unknown = from_name "?"

type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

let to_c_type (j:t) : C_type.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* ty = with_field "qualType" cast_string o in
  Ok (C_type.make ty)

let matches (f:C_type.t -> bool) (j:t) : bool =
  match to_c_type j with
  | Ok x -> f x
  | Error _ -> false

let to_string (j:t) : string =
  j
  |> to_c_type
  |> Result.map C_type.to_string
  |> Result.value ~default:"?"

