open Bank_conflicts
open OUnit2
open Protocols

let assert_bc ~expected ~given : unit =
  let msg =
    "Expected: " ^ Index_analysis.BC.to_string expected ^ "\n" ^
    "Given: " ^ Index_analysis.BC.to_string given
  in
  assert_equal expected given ~msg

let cfg =
  let block_dim = Dim3.make ~x:32 () in
  let grid_dim = Dim3.one in
  Config.make ~block_dim ~grid_dim ()

let bc_any ~expected ~given : unit =
  let given =
    Index_analysis.BC.from_nexp cfg Variable.Set.empty given
  in
  assert_bc ~expected:(expected, Any) ~given

let bc_uniform ~expected ~given : unit =
  assert_bc ~expected:(expected, Uniform)
    ~given:(Index_analysis.BC.from_nexp cfg Variable.Set.empty given)

let tests = "test_predicates" >::: [
  "imp_to_post_1" >:: (fun _ ->
    let open Exp in
    let tidx = Var Variable.tid_x in
    bc_any ~expected:tidx ~given:tidx;
    bc_uniform ~expected:(Num 10) ~given:(Num 10);
    bc_uniform ~expected:(Binary (Plus, Num 10, Num 20)) ~given:(Binary (Plus, Num 10, Num 20));
    bc_any
      ~expected:tidx
      ~given:(Binary (Plus, tidx, Num 20));
    bc_any
      ~expected:tidx
      ~given:(Binary (Minus, tidx, Num 20));
    bc_any ~expected:tidx ~given:(Binary (Plus, tidx, Num 20));
    bc_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, tidx, Num 20));
    bc_any ~expected:(Binary (Mult, tidx, Num 20)) ~given:(Binary (Mult, Binary (Plus, tidx, Num 5), Num 20));
    ()
  );
]

let _ = run_test_tt_main tests

