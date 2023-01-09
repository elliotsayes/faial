open Stage0
open Protocols

let rec to_maxima : Symbolic.t -> string =
  function
  | Const k -> string_of_int k
  | Sum (x, ub, s) -> "sum(" ^ to_maxima s ^ ", " ^ Variable.name x ^ ", 1, " ^ Exp.n_to_string ub ^ ")"
  | Add l -> List.map to_maxima l |> Common.join " + "

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

let run_symbolic ?(verbose=false) ?(exe="maxima") (x:Symbolic.t) : (string, Errors.t) Result.t =
  let expr = to_maxima x ^ ",simpsum;" in
  (if verbose
    then prerr_endline ("maxima output:\n" ^ expr ^ "\n")
    else ());
  Common.run ~stdin:expr ~exe ["--very-quiet"; "--disable-readline"]
  |> Errors.handle_result parse_maxima
