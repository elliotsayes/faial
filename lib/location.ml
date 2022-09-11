
module Position = struct
  (** The position is a 2D indexing in the screen buffer. It is 1-based. *)
  type t = {
    line: int;
    column: int;
  }

  let start : t = {line = 1; column=1}

  let to_pair (pos:t) : int * int = pos.line, pos.column

  let lt (p1:t) (p2:t) : bool = to_pair p1 < to_pair p2

  (* Return the line number and position of a position *)
  let from_lexing (pos:Lexing.position) : t =
    let open Lexing in
    {
      line = pos.pos_lnum;
      column = pos.pos_cnum - pos.pos_bol + 1
    }

  let compare (p1:t) (p2:t) : int =
    compare (to_pair p1) (to_pair p2)

  (** Prints a position *)

  let add_to ?(columns=0) ?(lines=0) (x:t) : t =
    { column = x.column + columns; line = x.line + lines; }

  let bprint (b:Buffer.t) (pos:t): unit =
    Printf.bprintf b "%d:%d" pos.line pos.column

  let to_string (pos:t) : string =
    let b = Buffer.create 256 in
    bprint b pos;
    Buffer.contents b

  let repr (pos:t) : string =
    "{line=" ^ string_of_int pos.line ^ ", " ^
    "column=" ^ string_of_int pos.column ^"}"

end

(** Represents a source code location. *)

type t = {
  filename: string;
  first : Position.t;
  last : Position.t;
}

let count_columns (x:t) : int =
  x.last.column - x.first.column

let make ~filename ~first ~last : t =
  {filename = filename; first = first; last = last}

let filename (x:t): string = x.filename

let first (x:t) : Position.t  = x.first

let last (x:t) : Position.t = x.last

let add_to_last ?(columns=0) ?(lines=0) (x:t) : t =
  { x with last = Position.add_to ~columns ~lines x.last }

let repr (l:t) : string =
  "{filename=\"" ^ l.filename ^ "\", " ^
  "first=" ^ Position.repr l.first ^ ", " ^
  "last=" ^ Position.repr l.last ^ "}"

let add (lhs:t) (rhs:t) : t =
  (* We place all the positions in a list,
     we sort the list, and
     we take the first and last. *)
  let positions =
    [lhs.first; lhs.last; rhs.first; rhs.last]
    |> List.sort Position.compare
  in
  {
    filename = lhs.filename;
    first = List.nth positions 0;
    last = List.nth positions 3;
  }

let lt (l1:t) (l2:t) : bool =
  Position.lt l1.first l2.first || (l1.first = l2.first && Position.lt l1.last l2.last)

let empty : t = {filename = ""; first = Position.start; last = Position.start}

let from_lexing_pair ((p_start:Lexing.position), (p_end:Lexing.position)) : t =
  let open Lexing in
  {
    filename = p_start.pos_fname;
    first = p_start |> Position.from_lexing;
    last = p_end |> Position.from_lexing;
  }

let of_lexbuf lb =
  let open Lexing in
  from_lexing_pair (lb.lex_start_p, lb.lex_curr_p)

let line_range filename offset count =
  (* Skip the first n-lines *)
  let rec skip_n ic count =
    if count <= 0 then ()
    else begin
      let _ = input_line ic in
      skip_n ic (count - 1)
    end
  in
  (* Return the first n-lines *)
  let yield_n ic count =
    List.init count (fun _ -> input_line ic)
  in
  let ic = open_in filename in
  skip_n ic offset;
  let lines = yield_n ic count in
  close_in ic;
  lines

let get_line filename offset =
  match line_range filename offset 1 with
  | [l] -> l
  | _ -> failwith "Unexpected output"

let line_count loc =
  loc.last.line - loc.first.line

let start_offset loc = loc.first.line - 1

(** Returns a list of the lines that comprise the location. *)

let lines (loc:t) =
  line_range
    loc.filename
    (start_offset loc)
    (line_count loc)

(** Returns the first line of location *)

type range = {
  range_offset: int;
  range_count: int;
}

let location_title (loc:t) : string * range =
  let err_text = get_line loc.filename (start_offset loc) in
  let start_line, start_off = Position.to_pair loc.first in
  let start_idx = start_off - 1 in
  let end_line, end_off = Position.to_pair loc.last in
  let count =
    if start_line != end_line
    then String.length err_text
    else end_off - start_off
  in
  err_text, { range_offset = start_idx; range_count = count }

(** Prints the location; highlights the locations *)

let make_bold =
   ANSITerminal.sprintf [ANSITerminal.Bold; ANSITerminal.Foreground ANSITerminal.Red] "%s"

let location_bprint_title (outx:Buffer.t) (loc:t) : unit =
  (* Print out the line until you reach the highlighted text: *)
  let txt, hl = location_title loc in
  let lineno = loc.first.line in
  let left = String.sub txt 0 hl.range_offset in
  Printf.bprintf outx "%d | %s" lineno left;

  (* Print out the highlighted text *)
  let mid = String.sub txt hl.range_offset hl.range_count in
  Printf.bprintf outx "%s" (make_bold mid);

  (* Print out the rest of the string *)
  let idx = hl.range_offset + hl.range_count in
  let right = String.sub txt idx (String.length txt - idx) in
  Printf.bprintf outx "%s\n" right;

  (* Underline the highlighted text *)
  let spaces =
    let count = Printf.sprintf "%d | " lineno |> String.length in
    String.make count ' '
  in
  let underline offset count : string =
    let count = if count = 0 then 1 else count in
    (String.make offset ' ') ^ (String.make count '^' |> make_bold)
  in
  Printf.bprintf outx "%s%s\n" spaces (underline hl.range_offset hl.range_count)

let print_location (l:t) : unit =
  let out = Buffer.create 1024 in
  location_bprint_title out l;
  Buffer.output_buffer stdout out

let bprint_errs b (errs:(string * t option) list) : bool =
  let print_err (msg,loc:string * t option) =
    match loc with
    | Some loc -> Printf.bprintf b "%a: %s" (fun b x -> Position.bprint b x.first) loc msg;
      (try
          (Printf.bprintf b "\n\n%a" location_bprint_title loc)
        with Sys_error _ -> ())
    | None -> Printf.bprintf b "%s" msg
  in
  List.iter print_err errs;
  List.length errs > 0
