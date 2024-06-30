open Protocols

module Visibility = struct
  type t =
    | Global
    | Local (* cannot be bubbled up; works like if (!exp) return *)
  let to_string : t -> string =
    function
    | Global -> "global"
    | Local -> "local"
end

type t = {
  visibility: Visibility.t;
  cond: Exp.bexp;
}

let make (cond:Exp.bexp) (visibility:Visibility.t) : t =
  { cond; visibility }

let map (f:Exp.bexp -> Exp.bexp) (a:t) : t =
  { a with cond = f a.cond }

let to_string (a:t) : string =
  Visibility.to_string a.visibility ^ " " ^
  "assert(" ^ Exp.b_to_string a.cond ^ ")"
