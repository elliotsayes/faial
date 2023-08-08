open Protocols
open Drf

open OUnit2
open Exp

let x = (Variable.from_name "x")
let y = (Variable.from_name "y")
let b = Bool true
let r = Range.{
  var = Variable.from_name "z";
  lower_bound = Num 1;
  upper_bound = Num 2;
  step = Plus (Num 1);
  dir = Increase;
}
let write = Access.{
  index = [];
  mode = Mode.Wr;
}
let x_acc = Unsync.Acc (x, write)
let y_acc = Unsync.Acc (y, write)

let assert_filter i (expected:Unsync.inst option) =
  let given = Unsync.filter_by_location x i in
  let msg = match expected, given with
  | Some _, None -> "given none, expecting:"
  | None, Some _ -> "expecting none, given:"
  | Some l1, Some l2 -> "expecting:\n" ^ Unsync.to_string [l1] ^
    "\ngiven:\n" ^ Unsync.to_string [l2]
  | None, None -> ""
  in
  assert_equal given expected ~msg

let expect_none l =
  assert_filter (Cond (b, l)) None

let expect_some l1 l2 =
  assert_filter
    (Cond (b, l1))
    (Some (Cond (b, l2)))

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
        Assert b;
        y_acc
      ]
      [
        x_acc;
        Assert b;
      ];
    expect_some
      [
        x_acc;
        Assert b;
        Assert b;
        Assert b;
      ]
      [
        x_acc;
        Assert b;
        Assert b;
        Assert b;
      ];
    ()
  );
  "lvl2" >:: (fun _ ->
    expect_some
      [
        Cond (b, [x_acc]);
        y_acc
      ]
      [
        Cond (b, [x_acc])
      ];
    expect_some
      [
        Cond (b, [x_acc]);
        Assert b;
        y_acc
      ]
      [
        Cond (b, [x_acc]);
        Assert b;
      ];
    expect_some
      [
        Loop (r, [Cond (b, [
          x_acc
        ])
        ]);
        Assert b;
        y_acc
      ]
      [
        Loop (r, [Cond (b, [
          x_acc
        ])]);
        Assert b;
      ];
    expect_some
      [
        Loop (r, [Cond (b, [
          x_acc
        ])
        ]);
        Assert b;
        Assert b;
        Assert b;
        y_acc
      ]
      [
        Loop (r, [Cond (b, [
          x_acc
        ])]);
        Assert b;
        Assert b;
        Assert b;
      ];
    ()
  );
]

let _ = run_test_tt_main tests
