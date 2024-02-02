open Protocols

type t = {
  ty_var : Ty_variable.t;
  is_used: bool;
  is_shared: bool;
}

let make ~ty_var ~is_used ~is_shared : t =
  {ty_var; is_used; is_shared}

let ty_var (x:t) : Ty_variable.t = x.ty_var

let name (x:t) : Variable.t = x.ty_var.name

let has_type (type_of:C_type.t -> bool) (x:t) : bool =
  Ty_variable.has_type type_of x.ty_var

let to_string (p:t) : string =
  let used = if p.is_used then "" else " unsed" in
  let shared = if p.is_shared then "shared " else "" in
  used ^ shared ^ Ty_variable.to_string p.ty_var
