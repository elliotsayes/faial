open Stage0

module Mode = struct
  type t =
    | Rd
    | Wr of int option

  let to_string : t -> string =
    function
    | Rd -> "ro"
    | Wr (Some n) -> "rw(" ^ string_of_int n ^ ")"
    | Wr None -> "rw"

  let is_read : t -> bool =
    function
    | Rd -> true
    | Wr _ -> false

  let is_write : t -> bool =
    function
    | Rd -> false
    | Wr _ -> true

  let can_conflict (m1:t) (m2:t) : bool =
    match m1, m2 with
    | Rd, Rd -> false
    | Wr (Some x), Wr (Some y) -> x <> y
    | _, _ -> true
end

(* An access pairs the index-expression with the access mode (R/W) *)
type t = {index: Exp.nexp list; mode: Mode.t}

let mode (x:t) : Mode.t = x.mode

let is_write (x:t) : bool =
  Mode.is_write x.mode

let is_read (x:t) : bool =
  Mode.is_read x.mode

let map (f:Exp.nexp -> Exp.nexp) (a: t) : t =
  { a with index = List.map f a.index }

let to_string ?(name="") (a:t) : string =
  let index_to_s (ns:Exp.nexp list) : string =
    let idx =
      ns
      |> List.map Exp.n_to_string
      |> Common.join ", "
    in
      "[" ^ idx ^ "]"
  in
  Mode.to_string a.mode ^ " " ^ name ^ index_to_s a.index

let write (index:Exp.nexp list) (v:int option) : t =
  { index = index; mode = Wr v}

let read (index:Exp.nexp list) : t =
  { index = index; mode = Rd }

let can_conflict (a1:t) (a2:t) =
  Mode.can_conflict a1.mode a2.mode
