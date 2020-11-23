open OUnit2
open Phasealign
open Locsplit
open Exp

let x = (var_make "x")
let y = (var_make "y")
let b = Bool true
let r = {
  range_var = var_make "z";
  range_lower_bound = Num 1;
  range_upper_bound = Num 2;
  range_step = Default (Num 1)
}
let write = {
  access_index = [];
  access_mode = W;
}
let x_acc = UAcc (x, write)
let y_acc = UAcc (y, write)

let assert_filter i (expected:u_inst option) =
  let given = filter_by_location x i in
  let msg = match expected, given with
  | Some l, None -> "given none, expecting:"
  | None, Some l -> "expecting none, given:"
  | Some l1, Some l2 -> "expecting:\n" ^ u_prog_to_string [l1] ^
    "\ngiven:\n" ^ u_prog_to_string [l2]
  | None, None -> ""
  in
  assert_equal given expected ~msg

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
    expect_some
      [
        x_acc;
        UAssert b;
        UAssert b;
        UAssert b;
      ]
      [
        x_acc;
        UAssert b;
        UAssert b;
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
    expect_some
      [
        ULoop (r, [UCond (b, [
          x_acc
        ])
        ]);
        UAssert b;
        y_acc
      ]
      [
        ULoop (r, [UCond (b, [
          x_acc
        ])]);
        UAssert b;
      ];
    expect_some
      [
        ULoop (r, [UCond (b, [
          x_acc
        ])
        ]);
        UAssert b;
        UAssert b;
        UAssert b;
        y_acc
      ]
      [
        ULoop (r, [UCond (b, [
          x_acc
        ])]);
        UAssert b;
        UAssert b;
        UAssert b;
      ];
    ()
  );
]

let _ = run_test_tt_main tests
