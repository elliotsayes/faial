open Stage0
open Protocols

let brel_to_string : Exp.brel -> string =
  function
  | BOr -> "or"
  | BAnd -> "and"

let tr_op_to_string : Reals.tr_op -> string =
  function
  | Ceiling -> "ceiling"
  | Floor -> "floor"

let rec i_to_string : Reals.integer -> string =
  function
  | Var x -> Variable.name x
  | Num x -> string_of_int x
  | FloatToInt (o, e) -> tr_op_to_string o ^ "(" ^ f_to_string e ^ ")"
  | Bin (o, e1, e2) ->
    let e1 = i_to_string e1 in
    let e2 = i_to_string e2 in
    let infix =
      match o with
      | Plus | Minus | Mult | Div -> true
      | Mod | LeftShift | RightShift | BitXOr | BitOr | BitAnd -> false
    in
    let o =
      match o with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Mod -> "mod"
      | LeftShift -> "bit_lsh"
      | RightShift -> "bit_rsh"
      | BitXOr -> "bit_xor"
      | BitOr -> "bit_or"
      | BitAnd -> "bit_and"
    in
    if infix then
      "(" ^ e1 ^ o ^ e2 ^ ")"
    else
      o ^ "(" ^ e1 ^ ", " ^ e2 ^")"
  | BitNot e ->
    "bit_not(" ^ i_to_string e ^ ")"
  | NIf (b, e1, e2) ->
    "if is (" ^ b_to_string b ^ ") then " ^ i_to_string e1 ^ " else " ^
    i_to_string e2 ^")"
  | BoolToInt e -> i_to_string (NIf (e, Num 1, Num 0))
  | PowerOf (base, e) ->
    f_to_string (FUnop (Exponent, base, IntToFloat e))
and b_to_string : Reals.boolean -> string =
  function
  | Bool true -> "true"
  | Bool false -> "false"
  | NRel (o, e1, e2) ->
    "(" ^ i_to_string e1 ^ " " ^ Exp.nrel_to_string o ^
    " " ^ i_to_string e2 ^ ")"
  | BRel (o, e1, e2) ->
    "(" ^ b_to_string e1 ^ " " ^ Exp.brel_to_string o ^
    " " ^ b_to_string e2 ^ ")"
  | BNot e ->
    "not (" ^ b_to_string e ^ ")"
  | IntToBool e ->
    "bool(" ^ i_to_string e ^ ")"
and f_to_string : Reals.floating_point -> string =
  function
  | FBin (o, e1, e2) ->
    "(" ^ f_to_string e1 ^ " " ^ Reals.fbin_to_string o ^
    " " ^ f_to_string e2 ^ ")"
  | FUnop (Exponent, b, e) ->
    string_of_int b ^ "^(" ^ f_to_string e ^ ")"
  | FUnop (Logarithm, b, e) ->
    "log(" ^ f_to_string e ^ ")/log(" ^ string_of_int b ^ ")"
  | IntToFloat e ->
    i_to_string e

let rec from_symbolic : Symbolic.t -> string =
  function
  | Const k -> string_of_int k
  | Sum (b, s) ->
    "sum(" ^
      from_symbolic s ^ ", " ^
      Variable.name b.var ^ ", " ^
      i_to_string b.lower_bound ^ ", " ^
      i_to_string b.upper_bound ^
    ")"
  | Add l -> List.map from_symbolic l |> Common.join " + "

let from_ra (r: Ra.t) : (string, Errors.t) Result.t =
  Symbolic.Default.from_ra r
  |> Result.map from_symbolic
  |> Symbolic.adapt_error

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

let run ?(verbose=false) ?(exe="maxima") (expr:string) : (string, Errors.t) Result.t =
  let expr = "load(\"bitwise\")$\n" ^ expr ^ ",logcontract,simpsum,ratsimp;" in
  (if verbose
    then prerr_endline ("maxima output:\n" ^ expr ^ "\n")
    else ());
  Subprocess.make exe ["--very-quiet"; "--disable-readline"]
  |> Subprocess.run_combine ~stdin:expr
  |> Errors.handle_result parse_maxima

let run_ra_ratio
  ~verbose
  ~exe
  ~numerator
  ~denominator
:
  (string, Errors.t) Result.t
=
  let (let*) = Result.bind in
  if Ra.is_zero denominator then Ok "0" else
  let* numerator = from_ra numerator in
  let* denominator = from_ra denominator in
  run ~verbose ~exe ("(" ^ numerator ^ ") / (" ^ denominator ^ ")")

let run_ra ?(verbose=false) ?(exe="maxima") (x:Ra.t) : (string, Errors.t) Result.t =
  let (let*) = Result.bind in
  let* s = from_ra x in
  run ~verbose ~exe s
