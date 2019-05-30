{
open Lexing
open Parse2

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 1 *)
let uint = ['0'-'9'] ['0'-'9']*

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* part 4 *)
rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | uint     { UINT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | ';' { SEMICOLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | '<' { LT }
  | '>' { GT }
  | '!' { NOT }
  | ">=" { GTE }
  | "<=" { LTE }
  | "||" { OR }
  | "&&" { AND }
  | "==" { EQ }
  | "!=" { NEQ }
  | "sync" {SYNC}
  | "rw" {RW}
  | "ro" {RO}
  | "for" {FOR}
  | "if" { IF }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
