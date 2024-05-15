open Protocols
open Stage0

type t = {name: string; ty: C_type.t}

let to_string (d:t) =
  "typedef " ^ C_type.to_string d.ty ^ " " ^ d.name

let to_s (d:t) : Indent.t list =
  [Line (to_string d ^ ";")]
