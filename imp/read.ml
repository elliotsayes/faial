open Protocols

type t = {
  target: (C_type.t * Variable.t) option;
  array: Variable.t;
  index: Exp.nexp list
}

let to_acc (r:t) : Variable.t * Access.t =
  (r.array, Access.{index=r.index; mode=Read})

