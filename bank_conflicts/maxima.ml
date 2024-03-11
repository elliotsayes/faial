open Stage0
open Protocols

let rec from_symbolic : Symbolic.t -> string =
  function
  | Const k -> string_of_int k
  | Sum (b, s) ->
    "sum(" ^
      from_symbolic s ^ ", " ^
      Variable.name b.var ^ ", " ^
      Exp.n_to_string b.first_elem ^ ", " ^
      Exp.n_to_string b.last_elem ^
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
  let expr =
    "log2(x) := log(x)/log(2)$" ^
    "pow2(x) := x^2\n$" ^ expr ^ ",logcontract,simpsum,ratsimp;" in
  (if verbose
    then prerr_endline ("maxima output:\n" ^ expr ^ "\n")
    else ());
  Subprocess.make exe ["--very-quiet"; "--disable-readline"]
  |> Subprocess.capture ~stdin:expr
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
