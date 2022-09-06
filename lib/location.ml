
module Position = struct
  (** The position is a 2D indexing in the screen buffer. It is 1-based. *)
  type t = {
    filename: string;
    line: int;
    column: int;
  }

  let empty : t = {filename = ""; line = 1; column=1}

  let to_tuple (p:t) : (string * int * int) = (p.filename, p.line, p.column)

  let lt (p1:t) (p2:t) : bool = to_tuple p1 < to_tuple p2

  (* Return the line number and position of a position *)
  let from_lexing (pos:Lexing.position) : t =
    let open Lexing in
    {
      filename = pos.pos_fname;
      line = pos.pos_lnum;
      column = pos.pos_cnum - pos.pos_bol + 1
    }

  let to_pair (pos:t) = pos.line, pos.column

  (** Prints a position *)

  let bprint (b:Buffer.t) (pos:t): unit =
    let fname = if pos.filename = ""
      then ""
      else pos.filename ^ ":"
    in
    Printf.bprintf b "%s%d:%d" fname pos.line pos.column

  let to_string (pos:t) : string =
    let b = Buffer.create 256 in
    bprint b pos;
    Buffer.contents b

  let repr (pos:t) : string =
    "{filename=\"" ^ pos.filename ^ "\", " ^
    "line=" ^ string_of_int pos.line ^ ", " ^
    "column=" ^ string_of_int pos.column ^"}"

end

(** Represents a source code location. *)

type t = {
  first : Position.t;
  last : Position.t;
}

let make ~first ~last : t = {first = first; last = last}

let first (x:t) = x.first

let last (x:t) = x.last

let location_repr (l:t) : string =
  "{first=" ^ Position.repr l.first ^ ", " ^
   "last=" ^ Position.repr l.last ^ "}"

let lt (l1:t) (l2:t) : bool =
  Position.lt l1.first l2.last || (l1.first = l2.first && Position.lt l1.last l2.last)

let empty : t = {first = Position.empty; last = Position.empty}

let of_lex_position_pair ((p_start:Lexing.position), (p_end:Lexing.position)) : t =
  let open Lexing in
  {
    first = p_start |> Position.from_lexing;
    last = p_end |> Position.from_lexing;
  }

let of_lexbuf lb =
  let open Lexing in
  of_lex_position_pair (lb.lex_start_p, lb.lex_curr_p)


(** Prints the start of the file location:
    filename:start-line:start-col *)
(*
let location_bprint_start (b:Buffer.t) (loc:t) : unit =
  Position.(bprint b loc.start)
*)

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

let location_line_count loc =
  loc.last.line - loc.first.line

let location_start_offset loc = loc.first.line - 1

(** Returns a list of the lines that comprise the location. *)

let location_lines (loc:t) =
  line_range
    loc.first.filename
    (location_start_offset loc)
    (location_line_count loc)

(** Returns the first line of location *)

type range = {
  range_offset: int;
  range_count: int;
}


let location_title (loc:t) =
  let err_text = get_line loc.first.filename (location_start_offset loc) in
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
  let underline offset count : string =
    let count = if count = 0 then 1 else count in
    (String.make offset ' ') ^ (String.make count '^' |> make_bold)
  in
  let txt, hl = location_title loc in
  let lineno = loc.first.line in
  let left = String.sub txt 0 hl.range_offset in
  Printf.bprintf outx "%d | %s" lineno left;
  let mid = String.sub txt hl.range_offset hl.range_count in
  let spaces =
    let count = Printf.sprintf "%d | " lineno |> String.length in
    String.make count ' '
  in
  Printf.bprintf outx "%s" (make_bold mid);
  let idx = hl.range_offset + hl.range_count in
  let right = String.sub txt idx (String.length txt - idx) in
  Printf.bprintf outx "%s\n" right;
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
