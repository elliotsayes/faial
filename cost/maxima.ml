open Stage0
open Protocols

let brel_to_string : B_rel.t -> string =
  function
  | BOr -> "or"
  | BAnd -> "and"

let tr_op_to_string : Reals.TruncateOp.t -> string =
  function
  | Ceiling -> "ceiling"
  | Floor -> "floor"

let rec i_to_string : Reals.integer -> string =
  function
  | Var x -> Variable.name x
  | Num x -> string_of_int x
  | FloatToInt (o, e) -> tr_op_to_string o ^ "(" ^ f_to_string e ^ ")"
  | Binary (Div, e1, e2) ->
    "floor(" ^ i_to_string e1 ^ "/" ^ i_to_string e2 ^ ")"
  | Binary (o, e1, e2) ->
    let e1 = i_to_string e1 in
    let e2 = i_to_string e2 in
    let infix =
      match o with
      | Plus | Minus | Mult | Div | Pow -> true
      | Mod | LeftShift | RightShift | BitXOr | BitOr | BitAnd -> false
    in
    let o =
      match o with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/" (* XXX: integer division *)
      | Mod -> "mod"
      | LeftShift -> "bit_lsh"
      | RightShift -> "bit_rsh"
      | BitXOr -> "bit_xor"
      | BitOr -> "bit_or"
      | BitAnd -> "bit_and"
      | Pow -> "^"
    in
    if infix then
      "(" ^ e1 ^ o ^ e2 ^ ")"
    else
      o ^ "(" ^ e1 ^ ", " ^ e2 ^")"
  | Unary (BitNot, e) ->
    "bit_not(" ^ i_to_string e ^ ")"
  | Unary (Negate, e) ->
    "-(" ^ i_to_string e ^ ")"
  | If (b, e1, e2) ->
    "(if (" ^ b_to_string b ^ ") then " ^ i_to_string e1 ^ " else " ^
    i_to_string e2 ^")"
  | BoolToInt e -> i_to_string (If (e, Num 1, Num 0))
and b_to_string : Reals.boolean -> string =
  function
  | Bool true -> "true"
  | Bool false -> "false"
  | NRel (NEq, e1, e2) ->
    "equal(" ^ i_to_string e1 ^ ", " ^ i_to_string e2 ^ ")"
  | NRel (NNeq, e1, e2) ->
    "notequal(" ^ i_to_string e1 ^ ", " ^ i_to_string e2 ^ ")"
  | NRel (o, e1, e2) ->
    "(" ^ i_to_string e1 ^ " " ^ N_rel.to_string o ^
    " " ^ i_to_string e2 ^ ")"
  | BRel (BOr, e1, e2) ->
    "(" ^ b_to_string e1 ^ " or " ^ b_to_string e2 ^ ")"
  | BRel (BAnd, e1, e2) ->
    "(" ^ b_to_string e1 ^ " and " ^ b_to_string e2 ^ ")"
  | BNot e ->
    "not (" ^ b_to_string e ^ ")"
  | IntToBool e ->
    "notequal(" ^ i_to_string e ^ ", 0)"
and f_to_string : Reals.floating_point -> string =
  function
  | Float f -> string_of_float f
  | Log (b, e) ->
    "log(" ^ f_to_string e ^ ")/log(" ^ i_to_string b ^ ")"
  | IntToFloat e ->
    i_to_string e

let rec from_summation : Summation.t -> string =
  function
  | Const k -> string_of_int k
  | If (b, p, q) ->
    "(if (" ^ b_to_string b ^ ") then " ^
      from_summation p ^ " else " ^ from_summation q ^ ")"
  | Sum (b, s) ->
    "sum(" ^
      from_summation s ^ ", " ^
      Variable.name b.var ^ ", " ^
      i_to_string b.lower_bound ^ ", " ^
      i_to_string b.upper_bound ^
    ")"
  | Plus (lhs, rhs) ->
    from_summation lhs ^ " + " ^ from_summation rhs

let from_stmt (r: Ra.Stmt.t) : string =
  Summation.from_stmt r |> from_summation

let parse_maxima (x:string) : string option =
  if Common.contains ~substring:"incorrect syntax" x then None
  else Some (
    let lines = String.split_on_char '\n' x in
    let max_len = List.map String.length lines
      |> List.fold_left max 0
    in
    let offsets =
      lines
      |> List.filter_map (fun line ->
        String.to_seqi line
        |> Seq.find (fun (_, a) -> a <> ' ')
        |> Option.map fst
      )
    in
    let min_offset = List.fold_left min max_len offsets in
    lines
    |> List.map (fun line ->
      Slice.from_start min_offset
      |> Slice.substring line
    )
    |> Common.join "\n"
  )

let compile (code:string) : string =
  "load(\"bitwise\")$\n" ^ code ^ ",logcontract,simpsum,ratsimp;"

let run_exe ?(verbose=false) ?(exe="maxima") (expr:string) : (string, Errors.t) Result.t =
  (if verbose
    then prerr_endline ("maxima output:\n" ^ expr ^ "\n")
    else ());
  Subprocess.make exe ["--very-quiet"; "--disable-readline"]
  |> Subprocess.run_combine ~stdin:expr
  |> Errors.handle_result parse_maxima

let run_ratio
  ~verbose
  ~exe
  ~numerator
  ~denominator
:
  (string, Errors.t) Result.t
=
  if Ra.Stmt.is_zero denominator then Ok "0" else
  "(" ^
    from_stmt numerator ^ ") / (" ^
    from_stmt denominator ^ ")"
  |> compile
  |> run_exe ~verbose ~exe

let run
  ?(verbose=false)
  ?(exe="maxima")
  (x:Ra.Stmt.t)
:
  (string, Errors.t) Result.t
=
  x
  |> from_stmt
  |> compile
  |> run_exe ~verbose ~exe
