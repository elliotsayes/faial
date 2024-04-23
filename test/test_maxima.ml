open Protocols
open Bank_conflicts
open Reals

let compile_eval
  ?(assigns=[])
  (e:Reals.t)
:
  string
=
  let decls =
    assigns
    |> List.map (fun (key, v) ->
        key ^ ": " ^ string_of_int v ^ "$"
      )
    |> String.concat "\n"
  in
  decls ^ "\n" ^ "\nev(" ^ Maxima.i_to_string e ^ ");"

let run_eval
  ?(assigns=[])
  (e:Reals.t)
:
  (string, Errors.t) Result.t
=
  e
  |> compile_eval ~assigns
  |> Maxima.run


let var (x:string) : integer =
  Var (Variable.from_name x)

let tests =
  let x = 10 in
  [
    [], Num 1, "1";
    ["x", x], var "x", "10";
    ["x", x], Bin (Plus, var "x", Num 1), "11";
    ["x", x], Bin (LeftShift, var "x", Num 1), string_of_int (Int.shift_left x 1);
    ["x", x], Bin (RightShift, var "x", Num 1), string_of_int (Int.shift_right x 1);
    ["x", x], Bin (Mult, var "x", Num 13), string_of_int (x * 13);
    ["x", x], Bin (Minus, var "x", Num 13), "- 3";
    ["x", x], Bin (Mod, var "x", Num 13), string_of_int (x mod 13);
    ["x", x], Bin (BitXOr, var "x", Num 13), string_of_int (Int.logxor x 13);
    ["x", x], Bin (BitOr, var "x", Num 13), string_of_int (Int.logor x 13);
    ["x", x], Bin (BitAnd, var "x", Num 13), string_of_int (Int.logand x 13);
    ["x", 2], Bin (Pow, var "x", Num 3), "8";
    (* Make sure we are working with integer division *)
    [], Bin (Div, Num 1, Bin (Plus, Num 0, Num 4)), "0";
  ]

let () =
  tests |> List.iter (fun (assigns, code, expected) ->
    let debug () =
      print_endline("Given expression: " ^ to_string code);
      print_endline("Assignments:");
      List.iter (fun (x,y) -> print_endline ("  " ^ x ^ " = " ^ string_of_int y)) assigns;
      print_endline("Generated maxima:\n" ^ compile_eval ~assigns code);
      print_endline("Expected output: " ^ expected)
    in
    match run_eval ~assigns code with
    | Ok given ->
      if expected <> given then (
        debug ();
        print_endline("Maxima output: " ^ given);
        exit 1
      ) else ()
    | Error e ->
      debug ();
      print_endline("Maxima error: " ^ Errors.to_string e);
      exit 1
  )
