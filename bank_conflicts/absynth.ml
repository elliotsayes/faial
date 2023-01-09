open Stage0
open Protocols

let from_symbolic : Symbolic.t -> string =
  let indent (depth:int) : string = String.make depth '\t' in
  let n_to_s = Exp.n_to_string in
  let rec translate (depth:int) : Symbolic.t -> string =
    function
    | Const k -> indent depth ^ "tick " ^ string_of_int k ^ "\n"
    | Sum (x, ub, s) ->
      indent depth ^ Variable.name x ^ " = 0\n" ^
      indent depth ^ "while " ^ Variable.name x ^ " < (" ^ n_to_s ub ^ "):\n" ^
      indent (depth + 1) ^ Variable.name x ^ " = " ^ Variable.name x ^ " + 1\n" ^
      translate (depth + 1) s
    | Add l -> List.map (translate depth) l |> String.concat ""
  in
  fun x ->
    "def f():\n" ^
    translate 1 x

let from_ra : Ra.t -> string =
  let indent (depth:int) : string = String.make depth '\t' in
  let rec translate (depth:int) : Ra.t -> string =
    function
    | Skip -> indent depth ^ "tick 0\n"
    | Tick k -> indent depth ^ "tick " ^ string_of_int k ^ "\n"
    | Loop (r, s) ->
      indent depth ^ Variable.name r.var ^ " = " ^ Exp.n_to_string (Range.while_init r) ^ "\n" ^
      indent depth ^ "while " ^ Exp.b_to_string (Range.while_cond r) ^ ":\n" ^
      translate (depth + 1) s ^
      indent (depth + 1) ^ Variable.name r.var ^ " = " ^ Exp.n_to_string (Range.while_inc r) ^"\n"
    | Seq (p, q) ->
      translate depth p ^ translate depth q
  in
  fun x ->
    "def f():\n" ^
    translate 1 x

let with_tmp ~prefix ~suffix (f:string -> 'a) : 'a =
  let fname = Filename.temp_file prefix suffix in
  try
      let res = f fname in
      Sys.remove fname;
      res
  with ex ->
      Sys.remove fname;
      raise ex

let write_string ~filename ~data : unit =
  let oc = open_out filename in
  try
    output_string oc data;
    close_out oc
  with ex ->
    close_out oc;
    raise ex

let parse_absynth (x:string) : string option =
  let (let*) = Option.bind in
  let* x =
    String.split_on_char '\n' x
    |> List.find_opt (String.starts_with ~prefix:"    Bound:")
  in
  let* (_, x) = Common.split ':' x in
  Some (String.trim x)

let run ?(verbose=false) ?(exe="absynth") (data:string) : (string, Errors.t) Result.t =
  (if verbose
    then prerr_endline ("Absynth output:\n" ^ data ^ "\n")
    else ());
  with_tmp ~prefix:"absynth_" ~suffix:".imp" (fun filename ->
    write_string ~filename ~data;
    Common.run ~exe [filename]
  )
  |> Errors.handle_result parse_absynth

let run_symbolic ?(verbose=false) ?(exe="absynth") (x:Symbolic.t) : (string, Errors.t) Result.t =
  run ~verbose ~exe (from_symbolic x)

let run_ra ?(verbose=false) ?(exe="absynth") (x:Ra.t) : (string, Errors.t) Result.t =
  run ~verbose ~exe (from_ra x)
