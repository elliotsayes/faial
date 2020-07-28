open Sexplib
open Proto

exception ParseError of (string list)

type 'a parser = {is_valid: Sexp.t -> bool; run: Sexp.t -> 'a}

val parse_kernel : kernel parser
val parse_proto : prog parser
