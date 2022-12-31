open OUnit2
open Protocols
open Bank_conflicts

let assert_n_equal (e1:Exp.nexp) (e2:Exp.nexp) : unit =
  assert_equal e1 e2 ~printer:Serialize.PPrint.n_to_s
(*
let assert_outputs (expected:string) (given:Exp.nexp) : unit =
  assert_equal expected (Serialize.PPrint.n_to_s given)
*)
let tests = "tests" >::: [

  "err1" >:: (fun _ ->
    let open Exp in
    (*1 / (1024 * gridDim.x)*)
    let x = Variable.from_name "x" in
    let exp = Bin (Div, Num 1, Bin (Mult, Num 1024, Var x)) in
    match Poly.from_nexp x exp with
    | Some p -> assert_n_equal (
        Bin (Mult, Bin (Div, Num 1, Num 1024), Var x))
        (Poly.to_nexp x p)
    | None -> assert false
  );

]

let _ = run_test_tt_main tests
