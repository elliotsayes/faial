open Bank_conflicts
open OUnit2
open Protocols
open Index_analysis

let assert_offset ~expected ~given : unit =
  let msg =
    "Expected: " ^ Default.OffsetAnalysis.to_string expected ^ "\n" ^
    "Given: " ^ Default.OffsetAnalysis.to_string given
  in
  assert_equal expected given ~msg

let assert_index ~expected ~given : unit =
  assert_offset ~expected:(Index expected) ~given:(Default.OffsetAnalysis.from_nexp given)

let assert_offset ~expected ~given : unit =
  assert_offset ~expected:(Offset expected) ~given:(Default.OffsetAnalysis.from_nexp given)

let tests = "test_predicates" >::: [
  "imp_to_post_1" >:: (fun _ ->
    let open Exp in
    let tidx = Var Variable.tidx in
    assert_index ~expected:tidx ~given:tidx;
    assert_offset ~expected:(Num 10) ~given:(Num 10);
    assert_offset ~expected:(Bin (Plus, Num 10, Num 20)) ~given:(Bin (Plus, Num 10, Num 20));
    assert_index ~expected:tidx ~given:(Bin (Plus, tidx, Num 20));
    assert_index ~expected:(Bin (Mult, tidx, Num 20)) ~given:(Bin (Mult, tidx, Num 20));
    assert_index ~expected:(Bin (Mult, tidx, Num 20)) ~given:(Bin (Mult, Bin (Plus, tidx, Num 5), Num 20));
    ()
  );
]

let _ = run_test_tt_main tests

