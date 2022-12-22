type t = {x : int; y: int; z: int;}
let make ~x:x ~y:y ~z:z : t = {x=x; y=y; z=z}
let to_string (v:t) =
  "[" ^ string_of_int v.x ^
  ", " ^ string_of_int v.y ^
  ", " ^ string_of_int v.z ^ "]"
