open Protocols

type t = {
  var: Variable.t;
  lower_bound: Reals.t;
  upper_bound: Reals.t;
}

let make ~var ~lower_bound ~upper_bound : t =
  {var; lower_bound; upper_bound}

let to_string (b:t) : string =
  "{" ^ Variable.name b.var ^ " | " ^
    Reals.to_string b.lower_bound ^ " ≤ " ^
    Variable.name b.var ^ " ≤ " ^
    Reals.to_string b.upper_bound ^
  "}"


let subst (x : Variable.t * Reals.t) (s:t) : t =
  { s with
    lower_bound = Reals.subst x s.lower_bound;
    upper_bound = Reals.subst x s.upper_bound;
  }
(*
let to_range (b:t) : Range.t =
  {
    var = b.var;
    lower_bound = b.first_elem;
    upper_bound = b.last_elem;
    step = Plus (Num 1);
    dir = Range.Increase;
    ty = C_type.int;
  }
*)
