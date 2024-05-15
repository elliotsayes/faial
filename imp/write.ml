open Protocols

type t = {
  array: Variable.t;
  index: Exp.nexp list;
  payload: int option
}

let to_acc (w:t) : Variable.t * Access.t =
  (w.array, Access.{index=w.index; mode=Write w.payload})

