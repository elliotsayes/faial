type t = {
  value: int;
  state: Transaction.t option;
}

let value (e:t) : int = e.value

let from_int (value:int) : t =
  assert (value >= 0);
  { value; state=None }

let make (value:int) (state:Transaction.t) : t =
  assert (value >= 0);
  { value; state=Some state; }

let (<) (lhs:t) (rhs:t) : bool =
  lhs.value < rhs.value

let (>) (lhs:t) (rhs:t) : bool =
  lhs.value > rhs.value

let (<=) (lhs:t) (rhs:t) : bool =
  lhs.value <= rhs.value

let (>=) (lhs:t) (rhs:t) : bool =
  lhs.value >= rhs.value

let zero : t = from_int 0
