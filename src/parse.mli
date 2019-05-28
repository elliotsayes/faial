open Sexplib
open Proto

exception ParseError of (string list)

type 'a parser = {is_valid: Sexp.t -> bool; run: Sexp.t -> 'a}

val parse_kernel : kernel parser
val parse_proto : proto parser
val parse_stream : ((string * access_t) list) parser
