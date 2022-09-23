(* Represents an interval, can be represented as start + length or via two indices *)
type t = {start: Index.t; length: int}

let zero : t = {start=Index.zero; length=0}

let from_start (index:Index.t) : t = {start=index; length=0}

let from_range ~start ~length : t = {start=start; length=length}

let from_coordinates ~start ~finish : t =
  { start=start; length=Index.distance finish start }

(* Translate the interval by a certain amoint. *)
let translate (amount:int) (x:t) : t =
  { x with start = Index.add x.start amount }

let start (x:t) : Index.t = x.start

let finish (x:t) : Index.t = Index.add x.start x.length

let length (x:t) : int = x.length

let set_length (length:int) (x:t) =
  {x with length}

let to_coordinates (x:t) : Index.t * Index.t =
  x.start, finish x

let repr (x:t) : string =
  Printf.sprintf "{start=%s, length=%d}"
    (Index.repr x.start)
    x.length

let (+) (lhs:t) (rhs:t) : t =
  from_coordinates
    ~start:(Index.min lhs.start rhs.start)
    ~finish:(Index.max (finish lhs) (finish rhs))

(* Return a substring using an interval *)
let substring (x:string) (s:t) : string =
  String.sub x (Index.to_base0 s.start) s.length
