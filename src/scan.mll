{
open Lexing
open Parse2

exception SyntaxError of string

(** https://stackoverflow.com/questions/16503396/ *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = pos.pos_cnum;
    pos_lnum = pos.pos_lnum + 1;
  }

let set_filename lexbuf filename =
  lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with pos_fname = filename
  }

}
(* part 1 *)
let uint = ['0'-'9'] ['0'-'9']*

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = '$'?['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* part 4 *)
rule read = parse
  | "#|" { multiline_comment lexbuf; read lexbuf }
  | "#" { singleline_comment lexbuf; read lexbuf }
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | '1' { ONE }
  | '2' { TWO }
  | uint     { UINT (int_of_string (Lexing.lexeme lexbuf)) }
  | ';' { SEMICOLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | '<' { LT }
  | '>' { GT }
  | '!' { NOT }
  | '@' { AT }
  | ">=" { GTE }
  | "<=" { LTE }
  | "||" { OR }
  | "&&" { AND }
  | "==" { EQ }
  | "!=" { NEQ }
  | "sync" {SYNC}
  | "rw" {RW}
  | "ro" {RO}
  | "foreach" { FOREACH }
  | "if" { IF }
  | "shared" { LOCS }
  | "distinct" { DISTINCT }
  | "const" { CONST }
  | "assert" { ASSERT }
  | "global" { GLOBAL }
  | "local" { LOCAL }
  | ',' { COMMA }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and singleline_comment = parse
  | '\n'   { next_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }
and multiline_comment = parse
  | "|#"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { next_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }
