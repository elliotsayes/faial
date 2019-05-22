open Sexplib
open Proto

type 'a parser = {is_valid: Sexp.t -> bool; run: Sexp.t -> 'a}

val parse_proto : proto parser
val parse_stream : ((string * access_t) list) parser
