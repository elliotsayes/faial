open Protocols

type t = {
  target: Variable.t;
  ty: C_type.t;
  array: Variable.t;
  index: Exp.nexp list
}

let to_acc (r:t) : Variable.t * Access.t =
  (r.array, Access.{index=r.index; mode=Read})

