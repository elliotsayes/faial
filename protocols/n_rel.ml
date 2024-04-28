type t =
  | NEq
  | NNeq
  | NLt
  | NLe
  | NGt
  | NGe


let eval : t -> int -> int -> bool =
  function
  | NEq -> (=)
  | NNeq -> (<>)
  | NLe -> (<=)
  | NGe -> (>=)
  | NLt -> (<)
  | NGt -> (>)

let to_string : t -> string =
  function
  | NEq -> "=="
  | NLe -> "<="
  | NLt -> "<"
  | NGe -> ">="
  | NGt -> ">"
  | NNeq -> "!="
