open OUnit2
open Common

let tests = "ends_with" >::: [
  "ends_with" >:: (fun _ ->
      assert_bool "" (ends_with "foo" "");
      assert_bool "" (ends_with "foo" "o");
      assert_bool "" (ends_with "foo" "oo");
      assert_bool "" (ends_with "foo" "foo");
      assert_bool "" (not (ends_with "foo" "moo"))
  )
]

let _ = run_test_tt_main tests
