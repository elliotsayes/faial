open OUnit2
open Phasealign
open Locsplit
open Exp

let x = (var_make "x")
let y = (var_make "y")
let b = Bool true
let write = {
  access_index = [];
  access_mode = W;
}
let x_acc = UAcc (x, write)
let y_acc = UAcc (y, write)

let assert_filter i expected =
  assert_equal (filter_by_location x i) expected

let expect_none l =
  assert_filter (UCond (b, l)) None

let expect_some l1 l2 =
  assert_filter
    (UCond (b, l1))
    (Some (UCond (b, l2)))

let tests = "locsplit" >::: [
  "lvl1" >:: (fun _ ->
    (* Empty code, should return none *)
    expect_none [];
    (* Single access, should return same *)
    expect_some
      [x_acc]
      [x_acc];
    (* y-accesses are filtered out *)
    expect_none [
      y_acc;
    ];
    (* y-accesses are filtered out *)
    expect_some
      [
        x_acc;
        y_acc
      ]
      [
        x_acc
      ];
    expect_some
      [
        x_acc;
        UAssert b;
        y_acc
      ]
      [
        x_acc;
        UAssert b;
      ];
    ()
  );
  "lvl2" >:: (fun _ ->
    expect_some
      [
        UCond (b, [x_acc]);
        y_acc
      ]
      [
        UCond (b, [x_acc])
      ];
    expect_some
      [
        UCond (b, [x_acc]);
        UAssert b;
        y_acc
      ]
      [
        UCond (b, [x_acc]);
        UAssert b;
      ];
    ()
  );
]

let _ = run_test_tt_main tests
