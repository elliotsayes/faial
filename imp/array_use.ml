open Protocols

type t = {address: Variable.t; offset: Exp.nexp}

let to_string (l:t) : string =
  Variable.name l.address ^ ":(" ^ Exp.n_to_string l.offset ^ ")"
