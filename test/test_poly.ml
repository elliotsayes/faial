open OUnit2
open Protocols
open Bank_conflicts
open Reals
open Poly

let assert_n_equal (e1:Reals.t) (e2:Reals.t) : unit =
  assert_equal e1 e2 ~printer:Reals.to_string

let assert_s_equal (p1:string) (p2:string) : unit =
  assert_equal p1 p2 ~printer:(fun x -> x)

let assert_p_equal (p1:Poly.t) (p2:Poly.t) : unit =
  assert_equal p1 p2 ~printer:Poly.to_string ~cmp:Poly.equal

let assert_to_string expected (p:Poly.t) : unit =
  assert_s_equal expected (p |> Poly.optimize |> Poly.to_string)

let assert_from_list (expected:Poly.t) l : unit =
  assert_p_equal expected (l |> Poly.from_list)

let assert_poly x ~given ~generated : unit =
    match Poly.from_reals x given with
    | Some p ->
      assert_n_equal
        (generated |> Reals.optimize )
        (Poly.to_reals x p |> Reals.optimize )
    | None -> assert false

let tests = "tests" >::: [
  "to_string" >:: (fun _ ->
    assert_to_string "10" (Exp0 (Num 10));
    assert_to_string "10 + 2·x" (Exp1 {coefficient=Num 2; constant=Num 10});
    let ht = Hashtbl.create 10 in
    Hashtbl.replace ht 3 (Num 6);
    Hashtbl.replace ht 1 (Num 9);
    assert_to_string "9·x + 6·x³" (Many ht)
  );

  "from_list" >:: (fun _ ->
    assert_from_list (Exp0 (Num 10)) [(Num 10, 0)];
    assert_from_list (Exp1 {coefficient=Num 2; constant=Num 10})
        [(Num 2, 1); (Num 10, 0)];
    let ht = Hashtbl.create 10 in
    Hashtbl.replace ht 3 (Num 6);
    Hashtbl.replace ht 1 (Num 9);
    Hashtbl.replace ht (-1) (Num 10);
    assert_from_list (Many ht) [(Num 6, 3); (Num 9, 1); (Num 10, -1)]
  );

  "mult1" >:: (fun _ ->
    let p1 = [(Num 6, 3); (Num 9, 1); (Num 10, -1)] |> Poly.from_list in
    let expected = [
      Binary (Mult, Num 2, Num 6), 3;
      Binary (Mult, Num 2, Num 9), 1;
      Binary (Mult, Num 2, Num 10), -1;
    ] |> Poly.from_list in
    assert_p_equal expected (Poly.mult1 (Num 2) p1)
  );

  "add_exponent" >:: (fun _ ->
    let p1 = [(Num 6, 3); (Num 9, 1); (Num 10, -1)] |> Poly.from_list in
    let expected = [(Num 6, 5); (Num 9, 3); (Num 10, 1)] |> Poly.from_list in
    assert_p_equal expected (Poly.add_exponent 2 p1)
  );

  "mult2" >:: (fun _ ->
    let p1 = [(Num 6, 3); (Num 9, 1); (Num 10, -1)] |> Poly.from_list in
    let expected = [
      Binary (Mult, Num 2, Num 6), 5;
      Binary (Mult, Num 2, Num 9), 3;
      Binary (Mult, Num 2, Num 10), 1;
    ] |> Poly.from_list in
    assert_p_equal expected (Poly.mult2 (Num 2) 2 p1);
    let p1 = [Num 1, -1] |> Poly.from_list in
    let expected = [Num 1, 0] |> Poly.from_list in
    assert_p_equal expected (Poly.mult2 (Num 1) 1 p1)
  );

  "inverse" >:: (fun _ ->
    let p1 = [(Num 6, 3); (Num 9, 1); (Num 10, -1)] |> Poly.from_list in
    let expected = [
      (Binary (Div, Num 1, Num 6), -3);
      (Binary (Div, Num 1, Num 9), -1);
      (Binary (Div, Num 1, Num 10), 1)
    ] |> Poly.from_list in
    assert_p_equal expected (Poly.inverse p1);
    let p1 = Poly.from_list [ (Num 1, 1) ] in
    let expected = Poly.from_list [ (Binary (Div, Num 1, Num 1), -1) ] in
    assert_p_equal expected (inverse p1)
  );

  "div" >:: (fun _ ->
    let p1 = Poly.from_list [ (Num 1, 1) ] in
    let expected = Poly.from_list [ (Num 1, 0) ] in
    assert_p_equal expected (div p1 p1)
  );

  "p2" >:: (fun _ ->
    (*
      (2 * x^2 + 3) * (4 * x^1 + 5)
      = 2* x^2 * (4 * x^1 + 5) + 3 * (4 * x^1 + 5)
      = 8*x^3 + 10*x^2 + 12*x^1 + 15
     *)
    let p1 = Poly.from_list [(Num 2, 2); (Num 3, 0) ]  in
    let p2 = Poly.from_list [(Num 4, 1); (Num 5, 0) ] in
    assert_p_equal
      (Poly.from_list [
        Binary (Mult, Num 4, Num 2), 3;
        Binary (Mult, Num 5, Num 2), 2;
        Binary (Mult, Num 4, Num 3), 1;
        Binary (Mult, Num 5, Num 3), 0;
      ])
      (Poly.mult p1 p2)
  );

  "err1" >:: (fun _ ->
    (*1 / (1024 * gridDim.x)*)
    let x = Variable.from_name "x" in
    (* 1 / (1024 x) *)
    let given = Binary (Div, Num 1, Binary (Mult, Num 1024, Var x)) in
    let generated =
      Binary (
        Mult,
        Binary (Div, Num 1, Num 1024),
        Binary (Div, Num 1, Var x)
      )
    in
    assert_poly x ~given ~generated
  );

  "from_reals" >:: (fun _ ->
    let x = Variable.from_name "x" in
    let given = Binary (Div, Var x, Var x) |> Poly.from_reals x |> Option.get in
    let expected = Exp0 (Num 1) in
    assert_p_equal expected given
  );

  "to_nexp" >:: (fun _ ->
    let x = Variable.from_name "x" in
    let p1 = Poly.from_list [ (Num 1, 1) ] in
    assert_s_equal "1" (Poly.to_string (div p1 p1));
    let expected = Num 1 in
    assert_n_equal expected (Poly.to_reals x (div p1 p1) |> Reals.optimize)
  );

  "err2" >:: (fun _ ->
    let x = Variable.from_name "x" in
    let given = Binary (Div, Var x, Var x) in
    let generated = Num 1 in
    assert_poly x ~given ~generated
  );

]

let _ = run_test_tt_main tests
