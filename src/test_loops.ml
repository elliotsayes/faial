open OUnit2
open Loops
open Proto

let tests = "loops" >::: [
  "generate_test_names" >:: (fun _ ->
      assert_equal (generate_fresh_name (var_make "foo") (VarSet.of_list [(var_make "foo"); (var_make "bar")])) (var_make "foo1")
  )
]

let _ = run_test_tt_main tests

