open OUnit2

let assert_string_equal given expected =
  assert_equal given expected ~printer:(fun x -> x)

let assert_int_equal given expected =
  assert_equal given expected ~printer:string_of_int

let assert_position_equal given expected =
  assert_equal given expected ~printer:Sourceloc.position_repr

let assert_location_equal given expected =
  assert_equal given expected ~printer:Sourceloc.location_repr

let assert_var_equal given expected =
  assert_equal given expected ~printer:Exp.var_repr