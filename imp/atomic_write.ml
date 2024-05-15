open Protocols

type t = {
  target: Variable.t;
  ty:C_type.t;
  atomic: Atomic.t;
  array: Variable.t;
  index: Exp.nexp list
}

let to_acc (a:t) : Variable.t * Access.t =
  (a.array, Access.{index=a.index; mode=Atomic a.atomic})

