type t = {x : int; y: int; z: int;}
let make ?(x=1) ?(y=1) ?(z=1) () : t = {x=x; y=y; z=z}

let parse (s:string) : (t, string) Result.t =
  try
    match Yojson.Basic.from_string s with
    | `List [`Int x; `Int y; `Int z] -> Ok ({x; y; z})
    | `List [`Int x; `Int y] -> Ok {x; y; z=1}
    | `List [`Int x] | `Int x -> Ok {x; y=1; z=1}
    | `List [] -> Ok {x=1; y=1; z=1}
    | _ -> Error ("Expecting a number of a list of up to 3 numbers (eg, [x,y,z])")
  with
    _ -> Error "Error parsing dim3"

let to_list (d:t) : int list =
  [d.x; d.y; d.z]

let to_string (d:t) : string =
  match d with
  | { x=x; y=1; z=1} -> string_of_int x
  | { x=x; y=y; z=1} -> "[" ^ string_of_int x ^ ", " ^ string_of_int y ^ "]"
  | _ ->
    let x = string_of_int d.x in
    let y = string_of_int d.y in
    let z = string_of_int d.z in
    "[" ^ x ^ ", " ^ y ^ ", " ^ z ^ "]"

let to_assoc ?(prefix="") (d:t) : (string * int) list =
  [
    (prefix ^ "x", d.x);
    (prefix ^ "y", d.y);
    (prefix ^ "z", d.z)
  ]
