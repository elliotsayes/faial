open OUnit2
open Predicates

type vrange = {
  vr_lower_bound: int;
  vr_upper_bound: int;
  vr_step: int;
}

let range (lb:int) (ub:int) (s:int) : int list =
  let rec iter n =
    if n < ub then
      n :: iter (n + s)
    else
      []
  in
  iter lb

let vrange_to_list (r:vrange) : int list =
  range r.vr_lower_bound r.vr_upper_bound r.vr_step

let rec last (l:'a list) : 'a =
  match l with
  | [] -> failwith "last: empty list"
  | [x] -> x
  | _ :: l -> last l

let first (l:'a list) : 'a =
  match l with
  | [] -> failwith "first: empty list"
  | x :: _ -> x

let mk_range (r:vrange) : Exp.range =
  let open Exp in
  {
    range_var = var_make "x";
    range_lower_bound = Num r.vr_lower_bound;
    range_upper_bound = Num r.vr_upper_bound;
    range_step = Default (Num r.vr_step)
  }

let r_last (r:vrange) : int =
  let r = mk_range r in
  let open Exp in
  n_eval (range_last r)

let v_last (r:vrange) : int =
  vrange_to_list r |> last

let assert_equal_int n1 n2 : unit =
  assert_equal n1 n2 ~printer:string_of_int

let assert_last (r:vrange) =
  assert_equal_int (v_last r) (r_last r)

let tests = "test_predicates" >::: [
  "last" >:: (fun _ ->
    assert_equal_int (last [1;2;3;4]) 4;
    ()
  );
  "range" >:: (fun _ ->
    assert_equal_int 0 (first (range 0 10 1));
    assert_equal_int 9 (last (range 0 10 1));
    assert_equal [0;1;2] (range 0 3 1);
    assert_equal [0;2] (range 0 3 2);
    ()
  );
  "base0" >:: (fun _ ->
    assert_last {vr_upper_bound = 10; vr_lower_bound = 0; vr_step = 1};
    assert_last {vr_upper_bound = 10; vr_lower_bound = 0; vr_step = 2};
    assert_last {vr_upper_bound = 10; vr_lower_bound = 0; vr_step = 3};
    assert_last {vr_upper_bound = 10; vr_lower_bound = 0; vr_step = 4};
    assert_last {vr_upper_bound = 10; vr_lower_bound = 0; vr_step = 5};
    ()
  );
  "base1" >:: (fun _ ->
    assert_last {vr_lower_bound = 1; vr_upper_bound = 10; vr_step = 1};
    assert_last {vr_lower_bound = 1; vr_upper_bound = 10; vr_step = 2};
    assert_last {vr_lower_bound = 1; vr_upper_bound = 10; vr_step = 3};
    assert_last {vr_lower_bound = 1; vr_upper_bound = 10; vr_step = 4};
    assert_last {vr_lower_bound = 1; vr_upper_bound = 10; vr_step = 5};
    ()
  );
  "base2" >:: (fun _ ->
    assert_last {vr_lower_bound = 2; vr_upper_bound = 10; vr_step = 1};
    assert_last {vr_lower_bound = 2; vr_upper_bound = 10; vr_step = 2};
    assert_last {vr_lower_bound = 2; vr_upper_bound = 10; vr_step = 3};
    assert_last {vr_lower_bound = 2; vr_upper_bound = 10; vr_step = 4};
    assert_last {vr_lower_bound = 2; vr_upper_bound = 10; vr_step = 5};
    ()
  );
]

let _ = run_test_tt_main tests
