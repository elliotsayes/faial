open Protocols

type t = {var: Variable.t; ty:C_type.t; init: Exp.nexp option}

let set ?(ty=C_type.int) (var: Variable.t) (init:Exp.nexp) : t =
  {init=Some init; ty; var}

let unset ?(ty=C_type.int) (var: Variable.t) : t =
  {init=None; ty; var}

let map (f:Exp.nexp -> Exp.nexp) (d:t) : t =
  { d with init = Option.map f d.init }

let to_string (d:t) : string =
  let ty = C_type.to_string d.ty in
  let x = Variable.name d.var in
  let init =
    d.init
    |> Option.map (fun n -> " = " ^ Exp.n_to_string n)
    |> Option.value ~default:""
  in
  let label =
    match Variable.label_opt d.var with
    | Some l -> " /* " ^ l ^ " */"
    | None -> ""
  in
  ty ^ " " ^ x ^ init ^ label
