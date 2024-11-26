open Bank_conflicts
open OUnit2
open Protocols

let assert_offset ~expected ~given : unit =
  let msg =
    "Expected: " ^ Offset_analysis.to_string expected ^ "\n" ^
    "Given: " ^ Offset_analysis.to_string given
  in
  assert_equal expected given ~msg

let cfg =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Config.make ~block_dim ~grid_dim ()

let assert_any ~expected ~given : unit =
  let given =
    Offset_analysis.from_nexp cfg Variable.Set.empty given
  in
  assert_offset ~expected:(expected, Any) ~given

let assert_uniform ~expected ~given : unit =
  assert_offset ~expected:(expected, Uniform)
    ~given:(Offset_analysis.from_nexp cfg Variable.Set.empty given)

let tests = "test_predicates" >::: [
  "imp_to_post_1" >:: (fun _ ->
    let open Exp in
    let tidx = Var Variable.tid_x in
    assert_any ~expected:tidx ~given:tidx;
    assert_uniform ~expected:(Num 10) ~given:(Num 10);
    assert_uniform ~expected:(Binary (Plus, Num 10, Num 20)) ~given:(Binary (Plus, Num 10, Num 20));
    assert_any
      ~expected:tidx
      ~given:(Binary (Plus, tidx, Num 20));
    assert_any
      ~expected:tidx
      ~given:(Binary (Minus, tidx, Num 20));
    assert_any ~expected:tidx ~given:(Binary (Plus, tidx, Num 20));
    assert_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, tidx, Num 20));
    assert_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, Binary (Plus, tidx, Num 5), Num 20));
    ()
  );
]

let _ = run_test_tt_main tests

