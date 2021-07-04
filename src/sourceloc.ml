open Hash_rt
open Ppx_compare_lib.Builtin

(** The position is a 2D indexing in the screen buffer. It is 1-based. *)
type position = {
  pos_line: int;
  pos_column: int;
}
[@@deriving hash, compare] (* Requires base *)

let pos_empty = {pos_line = 1; pos_column=1}

(* Return the line number and position of a position *)
let of_lex_position pos =
  let open Lexing in
  {
    pos_line = pos.pos_lnum;
    pos_column = pos.pos_cnum - pos.pos_bol + 1
  }

let pos_to_pair pos = pos.pos_line, pos.pos_column

(** Prints a position *)

let position_bprint b pos =
  Printf.bprintf b "%d:%d" pos.pos_line pos.pos_column

(** Represents a source code location. *)

type location = {
  loc_filename : string;
  loc_start : position;
  loc_end : position;
}
[@@deriving hash, compare]

let loc_empty = {
  loc_filename = "";
  loc_start = pos_empty;
  loc_end=pos_empty;
}

let of_lex_position_pair (p_start, p_end) =
  let open Lexing in
  {
    loc_filename = p_start.pos_fname;
    loc_start = p_start |> of_lex_position;
    loc_end = p_end |> of_lex_position;
  }

let of_lexbuf lb =
  let open Lexing in
  of_lex_position_pair (lb.lex_start_p, lb.lex_curr_p)


(** Prints the start of the file location:
    filename:start-line:start-col *)

let location_bprint_start (b:Buffer.t) loc =
  Printf.bprintf b "%s:%a" loc.loc_filename position_bprint loc.loc_start
(*
let location_fprint_start outx loc =
  let b = Buffer.create 1024 in
  location_bprint_start b loc;
  Buffer.output_buffer outx b

let location_to_string loc =
  let b = Buffer.create 1024 in
  location_bprint_start b loc;
  Buffer.contents b
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

let location_bprint_title (outx:Buffer.t) (loc:location) : unit =
  let underline offset count : string =
    (String.make offset ' ') ^ (String.make count '^' |> make_bold)
  in
  let txt, hl = location_title loc in
  let left = String.sub txt 0 hl.range_offset in
  Printf.bprintf outx "%s" left;
  let mid = String.sub txt hl.range_offset hl.range_count in
  Printf.bprintf outx "%s" (make_bold mid);
  let idx = hl.range_offset + hl.range_count in
  let right = String.sub txt idx (String.length txt - idx) in
  Printf.bprintf outx "%s\n" right;
  Printf.bprintf outx "%s\n" (underline hl.range_offset hl.range_count)
(*
let location_title_to_string (loc:location) : string =
  let b = Buffer.create 1024 in
  location_bprint_title b loc;
  Buffer.contents b


let location_fprint_title outx (loc:location) : unit =
  let b = Buffer.create 1024 in
  location_bprint_title b loc;
  Buffer.output_buffer outx b
*)

let bprint_errs b (errs:(string * location) list) : bool =
  let print_err (msg,loc:string * location) =
    Printf.bprintf b "%a: %s" location_bprint_start loc msg;
    try
      (Printf.bprintf b "\n\n%a" location_bprint_title loc)
    with Sys_error _ -> ()
  in
  List.iter print_err errs;
  List.length errs > 0
