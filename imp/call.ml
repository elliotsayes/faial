type t = {
  kernel: string;
  ty: string;
  args : Arg.t list
}

let kernel_id ~kernel ~ty : string =
  kernel ^ ":" ^ ty

let unique_id (c:t) : string =
  kernel_id ~kernel:c.kernel ~ty:c.ty

let to_string (c:t) : string =
  let args =
    c.args
    |> List.map Arg.to_string
    |> String.concat ", "
  in
  c.kernel ^ "(" ^ args ^ ")"
