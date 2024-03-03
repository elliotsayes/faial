open Protocols

type json = Yojson.Basic.t

(* Typed variable *)

type t = {name: Variable.t; ty: json}

let make ~name ~ty : t = {name; ty}

let name (x:t) : Variable.t = x.name

let is_tid (x:t) : bool =
  x.name |> Variable.is_tid

let ty (x:t) : json = x.ty

let to_string (x:t) : string =
  J_type.to_string x.ty ^ " " ^ Variable.name x.name

let has_type (pred:C_type.t -> bool) (x:t) : bool =
  J_type.to_c_type x.ty
  |> Result.map pred
  |> Result.value ~default:false
