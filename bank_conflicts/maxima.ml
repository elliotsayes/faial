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

let from_ra (r: Ra.t) : string =
  Symbolic.Default.from_ra r
  |> from_symbolic

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
  let expr = expr ^ ",simpsum,ratsimp;" in
  (if verbose
    then prerr_endline ("maxima output:\n" ^ expr ^ "\n")
    else ());
  Common.run ~stdin:expr ~exe ["--very-quiet"; "--disable-readline"]
  |> Errors.handle_result parse_maxima

let run_ra ?(verbose=false) ?(exe="maxima") (x:Ra.t) : (string, Errors.t) Result.t =
  run ~verbose ~exe (from_ra x)
