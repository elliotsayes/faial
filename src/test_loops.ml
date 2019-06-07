open OUnit2
open Loops

let tests = "loops" >::: [
  "generate_test_names" >:: (fun _ ->
      assert_equal (generate_fresh_name "foo" ["foo"; "bar"]) "foo1"
  )
]

let _ = run_test_tt_main tests
