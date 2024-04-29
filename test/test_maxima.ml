open Stage0
open Protocols
open Ra
open Reals

let compile
  (assigns:(string * int) list)
  (e:string)
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
  decls ^ "\n" ^ "\nev(" ^ e ^ ");"

let var (x:string) : integer =
  Var (Variable.from_name x)

let int_s (i:int) : string =
  string_of_int i |> Str.replace_first (Str.regexp_string "-") "- "

let bool_s (b:bool) : string =
  if b then "true" else "false"

let _float_s (i:float) : string =
  string_of_float i |> Str.replace_first (Str.regexp_string "-") "- "

let x = 10
let x_ = var "x"

let i_tests =
  [
    x_, x;
    Num 1, 1;

    FloatToInt (Floor, Float 1.9), Float.floor 1.9 |> Int.of_float;
    FloatToInt (Ceiling, Float 1.9), Float.ceil 1.9 |> Int.of_float;

    Binary (Plus, x_, Num 1), x + 1;
    Binary (LeftShift, x_, Num 1), Int.shift_left x 1;
    Binary (RightShift, x_, Num 1), Int.shift_right x 1;
    Binary (Mult, x_, Num 13), x * 13;
    Binary (Minus, x_, Num 13), x - 13;
    Binary (Mod, x_, Num 13), x mod 13;
    Binary (BitXOr, x_, Num 13), Int.logxor x 13;
    Binary (BitOr, x_, Num 13), Int.logor x 13;
    Binary (BitAnd, x_, Num 13), Int.logand x 13;
    Binary (Pow, Num 2, Num 3), 8;
    (* Make sure we are working with integer division *)
    Binary (Div, Num 1, Binary (Plus, Num 0, Num 4)), 1 / (0 + 4);

    Unary (BitNot, Num 10), Int.lognot 10;

    Unary (Negate, Num 10), (-10);

    If (Bool true, Num 2, Num 3), 2;

    BoolToInt true_, 1;
  ]

let b_tests : (Reals.boolean * bool) list =
  [
    Bool true, true;
    Bool false, false;

    NRel (NEq, Num 1, Num 2), 1 = 2;
    NRel (NNeq, Num 1, Num 2), 1 <> 2;
    NRel (NGt, Num 1, Num 2), 1 > 2;
    NRel (NGe, Num 1, Num 2), 1 >= 2;
    NRel (NLt, Num 1, Num 2), 1 < 2;
    NRel (NLe, Num 1, Num 2), 1 <= 2;

    BRel (BOr, true_, false_), true || false;
    BRel (BAnd, true_, false_), true && false;

    BNot true_, not true;
    BNot false_, not false;

    IntToBool (Num 10), true;
  ]

let f_tests : (Reals.floating_point * string) list =
  [
    Float 1.1, "1.1b0";
    Log (Num 2, Float 3.0), "1.584962500721156b0";
    IntToFloat(Num 10), "1.0b1";
  ]

let unit_test
  ~given_expr
  ~given_maxima
  ~expected_output
  ~assigns
:
  unit
=
  let generated = compile assigns given_maxima in
  let debug () =
    print_endline("Given expression: " ^ given_expr);
    print_endline("Assignments:");
    List.iter (fun (x,y) -> print_endline ("  " ^ x ^ " = " ^ string_of_int y)) assigns;
    print_endline("Generated maxima:\n" ^ generated);
    print_endline("Expected output: " ^ expected_output)
  in
  match generated |> Maxima.run_exe with
  | Ok given_output ->
    if expected_output <> given_output then (
      debug ();
      print_endline("Maxima output: " ^ given_output);
      exit 1
    ) else ()
  | Error e ->
    debug ();
    print_endline("Maxima error: " ^ Errors.to_string e);
    exit 1

let all_tests () : unit =
  let assigns = ["x", x] in
  i_tests |> List.iter (fun (code, expected) ->
    unit_test
      ~given_expr:(Reals.to_string code)
      ~given_maxima:(Maxima.i_to_string code)
      ~expected_output:(int_s expected)
      ~assigns
  );
  b_tests |> List.iter (fun (code, expected) ->
    unit_test
      ~given_expr:(Reals.b_to_string code)
      ~given_maxima:("is(" ^ (Maxima.b_to_string code) ^ ")")
      ~expected_output:(bool_s expected)
      ~assigns:[]
  );
  f_tests |> List.iter (fun (code, expected) ->
    unit_test
      ~given_expr:(Reals.f_to_string code)
      ~given_maxima:("bfloat(" ^ Maxima.f_to_string code ^ ")")
      ~expected_output:expected
      ~assigns:[]
  )

let () =
  match Subprocess.make "maxima" ["--version"] |> Subprocess.check_output with
  | Some maxima_version ->
    print_endline ("Using: " ^ String.trim maxima_version);
    all_tests ()
  | None ->
    print_endline "MAXIMA NOT FOUND, tests skiped"
