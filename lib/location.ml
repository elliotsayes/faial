(** Represents a source code location. *)

type t = {
  filename: string;
  line: int;
  column: int;
  length: int;
}

let make ~filename ~line ~column ~length : t =
  {filename = filename; line=line; column=column; length = length}

let from_position (pos:Lexing.position) : t =
  let open Lexing in
  {
    filename = pos.pos_fname;
    line = pos.pos_lnum;
    column = pos.pos_cnum - pos.pos_bol + 1;
    length = 0;
  }

let from_pair (filename:string) ((p1,p2):t * t) : t =
  let (column, length) =
    if p1.filename = p2.filename && p1.line = p2.line then (
      let first = min p1.column p2.column in
      let last = max p1.column p2.column in
      (first, last - first)
    ) else
      (p1.column, 0)
  in
  make
    ~filename:p1.filename
    ~line:p1.line
    ~column
    ~length

let filename (x:t): string = x.filename
let length (x:t) : int = x.length
let set_length (len:int) (x:t) : t =
  { x with length = len }
let line (x:t) : int = x.line
let column (x:t) : int = x.column

let repr (l:t) : string =
  "{filename=\"" ^ filename l ^ "\", " ^
  "line=" ^ (line l |> string_of_int) ^ ", " ^
  "col=" ^ (column l |> string_of_int) ^ ", " ^
  "len=" ^ (length l |> string_of_int) ^ "}"


let add (lhs:t) (rhs:t) : t =
  if lhs.filename <> rhs.filename || lhs.line <> rhs.line then
    lhs
  else
    let column = min lhs.column rhs.column in
    let length = max (lhs.column + lhs.length) (rhs.column + rhs.length) - column in
    {
      lhs with column; length
    }

let to_tuple (l1:t) : int * int * int = (line l1, column l1, length l1)

let lt (l1:t) (l2:t) : bool =
  l1.filename = l2.filename && to_tuple l1 < to_tuple l2

let empty : t = make ~filename:"" ~line:1 ~column:1 ~length:0

let from_lexing_pair ((p_start:Lexing.position), (p_end:Lexing.position)) : t =
  from_pair p_start.pos_fname (from_position p_start, from_position p_end)

let from_lexbuf (lb:Lexing.lexbuf) : t =
  let open Lexing in
  from_lexing_pair (lb.lex_start_p, lb.lex_curr_p)

let line_offset (x:t) : int = x.line - 1

let column_offset (x:t) : int = x.column - 1

(** Returns a list of the lines that comprise the location. *)
let get_line (loc:t) : string =
  Common.get_line
    (line_offset loc)
    loc.filename

let split (loc:t) (line:string) : string * string * string =
  let left = column_offset loc in
  let mid = left + loc.length in
  (Slice.from ~start:0 ~finish:left |> Slice.string line,
   Slice.from ~start:left ~finish:mid |> Slice.string line,
   Slice.from_start mid |> Slice.string line)
