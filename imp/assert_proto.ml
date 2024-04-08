open Protocols
open Stage0

type t =
  | Acc of Variable.t * Access.t
  | Assert of (Exp.bexp * t)
  | Sync of Location.t option
  | If of Exp.bexp * t * t
  | For of Range.t * t
  | Seq of t * t
  | Skip
  | Decl of {var: Variable.t; ty:C_type.t; body: t}

let to_string: t -> string =
  let rec to_s : t -> Indent.t list =
    function
    | Skip -> [Line "skip;"]
    | Sync _ -> [Line "sync;"]
    | Assert (b, s) ->
      [
        Line ("assert (" ^ Exp.b_to_string b ^ ") {");
        Block (to_s s);
        Line ("}");
      ]
    | Acc (x, e) -> [Line (Access.to_string ~name:(Variable.name x) e)]
    | Decl d ->
      [
        Line (C_type.to_string d.ty ^ " " ^ Variable.name d.var ^ " {");
        Block (to_s d.body);
        Line "}";
      ]

    | If (b, s1, s2) -> [
        Line ("if (" ^ Exp.b_to_string b ^ ") {");
        Block (to_s s1);
        Line "} else {";
        Block (to_s s2);
        Line "}"
      ]

    | For (r, s) -> [
        Line ("foreach (" ^ Range.to_string r ^ ") {");
        Block (to_s s);
        Line ("}")
      ]
    | Seq (p, q) ->
      to_s p @ to_s q
  in
  fun p -> to_s p |> Indent.to_string
