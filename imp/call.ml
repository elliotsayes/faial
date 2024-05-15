open Protocols

type t = {
  kernel: string;
  ty: string;
  args : (Variable.t * Arg.t) list
}

let kernel_id ~kernel ~ty : string =
  kernel ^ ":" ^ ty

let unique_id (c:t) : string =
  kernel_id ~kernel:c.kernel ~ty:c.ty

let to_string (c:t) : string =
  let args =
    c.args
    |> List.map (fun (k, a) ->
      Variable.name k ^ "=" ^ Arg.to_string a
    )
    |> String.concat ", "
  in
  c.kernel ^ "(" ^ args ^ ")"
