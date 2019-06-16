
(** The position is a 2D indexing in the screen buffer. It is 1-based. *)
type position = {
  pos_line: int;
  pos_column: int;
}

(* Return the line number and position of a position *)
let of_lexing_position pos =
  let open Lexing in
  {
    pos_line = pos.pos_lnum;
    pos_column = pos.pos_cnum - pos.pos_bol + 1
  }

let pos_to_pair pos = pos.pos_line, pos.pos_column

(** Prints a position *)

let position_print outx pos =
  Printf.fprintf outx "%d:%d" pos.pos_line pos.pos_column

(** Represents a source code location. *)

type location = {
  loc_filename : string;
  loc_start : position;
  loc_end : position;
}

let of_lexbuf lb =
  let open Lexing in
  {
    loc_filename = lb.lex_start_p.pos_fname;
    loc_start = lb.lex_start_p |> of_lexing_position;
    loc_end = lb.lex_curr_p |> of_lexing_position;
  }

(** Prints the start of the file location:
    filename:start-line:start-col *)

let location_print_start outx loc =
  Printf.fprintf outx "%s:%a" loc.loc_filename position_print loc.loc_start


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
  let rec yield_n ic count =
    List.init count (fun n -> input_line ic)
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
  loc.loc_end.pos_line - loc.loc_start.pos_line

let location_start_offset loc = loc.loc_start.pos_line - 1

(** Returns a list of the lines that comprise the location. *)

let location_lines loc =
  line_range
    loc.loc_filename
    (location_start_offset loc)
    (location_line_count loc)

(** Returns the first line of location *)

type range = {
  range_offset: int;
  range_count: int;
}


let location_title loc =
  let err_text = get_line loc.loc_filename (location_start_offset loc) in
  let start_line, start_off = pos_to_pair loc.loc_start in
  let start_idx = start_off - 1 in
  let end_line, end_off = pos_to_pair loc.loc_end in
  let count =
    if start_line != end_line
    then String.length err_text
    else end_off - start_off
  in
  err_text, { range_offset = start_idx; range_count = count }

(** Prints the location; highlights the locations *)

let make_bold =
   ANSITerminal.sprintf [ANSITerminal.Bold] "%s"

let location_print_title outx loc =
  let underline offset count : string =
    (String.make offset ' ') ^ (String.make count '^' |> make_bold)
  in
  let txt, hl = location_title loc in
  let left = String.sub txt 0 hl.range_offset in
  Printf.fprintf outx "%s" left;
  let mid = String.sub txt hl.range_offset hl.range_count in
  Printf.fprintf outx "%s" (make_bold mid);
  let idx = hl.range_offset + hl.range_count in
  let right = String.sub txt idx (String.length txt - idx) in
  Printf.fprintf outx "%s\n" right;
  Printf.fprintf outx "%s\n" (underline hl.range_offset hl.range_count)

