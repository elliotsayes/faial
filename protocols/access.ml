open Stage0

module Mode = struct
  type t =
    | Read
    (* The payload of the write is used
       to detect benign data-races.
       See can-conflict for potentially
       racy accesses. *)
    | Write of int option
    | Atomic

  let to_string : t -> string =
    function
    | Read -> "ro"
    | Write (Some n) -> "rw(" ^ string_of_int n ^ ")"
    | Write None -> "rw"
    | Atomic -> "atomic"

  let is_read : t -> bool =
    function
    | Read -> true
    | _ -> false

  let is_write : t -> bool =
    function
    | Write _ -> true
    | _ -> false

  let is_atomic : t -> bool =
    function
    | Atomic -> true
    | _ -> false

  let can_conflict (m1:t) (m2:t) : bool =
    match m1, m2 with
    | Read, Read | Atomic, Atomic -> false
    | Write (Some x), Write (Some y) -> x <> y
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

let index_to_string (ns:Exp.nexp list) : string =
  let idx =
    ns
    |> List.map Exp.n_to_string
    |> Common.join ", "
  in
    "[" ^ idx ^ "]"

let to_string ?(name="") (a:t) : string =
  Mode.to_string a.mode ^ " " ^ name ^ index_to_string a.index

let write (index:Exp.nexp list) (v:int option) : t =
  { index = index; mode = Write v}

let read (index:Exp.nexp list) : t =
  { index = index; mode = Read }

let atomic (index:Exp.nexp list) : t =
  { index = index; mode = Atomic }

let can_conflict (a1:t) (a2:t) =
  Mode.can_conflict a1.mode a2.mode
