open Protocols

type t =
  | TemplateType of Variable.t
  | NonTypeTemplate of {name: Variable.t; ty: J_type.t}

let to_string (p:t) : string =
  let name = match p with
  | TemplateType x -> x
  | NonTypeTemplate x -> x.name
  in
  Variable.name name

let name : t -> Variable.t =
  function
  | TemplateType x -> x
  | NonTypeTemplate x -> x.name
