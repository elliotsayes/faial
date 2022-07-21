open OUnit2
open Common


let tests = "tests" >::: [

  "app_rev" >:: (fun _ ->
    assert_equal [4; 3; 2; 1] (append_rev [1;2;3;4] []);
    assert_equal [3; 2; 1; 4] (append_rev [1;2;3] [4]);
    assert_equal [2; 1; 3; 4] (append_rev [1;2] [3;4]);
    assert_equal [1; 2; 3; 4] (append_rev [1] [2;3;4]);
    assert_equal [1; 2; 3; 4] (append_rev [] [1;2;3;4])
  );
  "app_tr" >:: (fun _ ->
    assert_equal [1; 2; 3; 4] (append_tr [1;2;3;4] []);
    assert_equal [1; 2; 3; 4] (append_tr [1;2;3] [4]);
    assert_equal [1; 2; 3; 4] (append_tr [1;2] [3;4]);
    assert_equal [1; 2; 3; 4] (append_tr [1] [2;3;4]);
    assert_equal [1; 2; 3; 4] (append_tr [] [1;2;3;4])
  );
  "ends_with" >:: (fun _ ->
      assert_bool "" (ends_with "" "foo");
      assert_bool "" (ends_with "o" "foo");
      assert_bool "" (ends_with "oo" "foo");
      assert_bool "" (ends_with "foo" "foo");
      assert_bool "" (not (ends_with "moo" "foo"))
  );
  "starts_with" >:: (fun _ ->
      assert_bool "" (starts_with "foo" "");
      assert_bool "" (starts_with "foo" "f");
      assert_bool "" (starts_with "foo" "fo");
      assert_bool "" (starts_with "foo" "foo");
      assert_bool "" (not (starts_with "foo" "moo"))
  );
  "contains" >:: (fun _ ->
    assert (contains ~needle:"abc" "abc");
    assert (contains ~needle:"" "abc");
    assert (contains ~needle:"a" "abc");
    assert (contains ~needle:"b" "abc");
    assert (contains ~needle:"c" "abc");
    assert (contains ~needle:"ab" "abc");
    assert (contains ~needle:"bc" "abc");
    assert (contains ~needle:"abc" "aaabc");
    assert (not (contains ~needle:"abc" "aaab"));
    assert (not (contains ~needle:"abc" "aabab"));
    assert (contains ~needle:"abc" "aababc");
  );
  "range" >:: (fun _ ->
    assert_equal [0] (range 0 0);
    assert_equal [0;1] (range 0 1);
    assert_equal [1;2;3] (range 1 3);
  );
  "hashtbl_elements" >:: (fun _ ->
    let ht = Hashtbl.create 0 in
    assert_equal (Common.hashtbl_elements ht) [];
    Hashtbl.add ht 0 true;
    assert_equal (Common.hashtbl_elements ht) [(0, true)];
    Hashtbl.add ht 1 false;
    let elems () =
      Common.hashtbl_elements ht
      |> List.sort (fun (x,_) (y,_) -> compare x y)
    in
    assert_equal (elems ()) [(0, true); (1, false)];
    Hashtbl.add ht 2 true;
    assert_equal (elems ()) [(0, true); (1, false); (2, true)]
  );
  "hashtbl_update" >:: (fun _ ->
    let ht = Hashtbl.create 0 in
    let elems () =
      Common.hashtbl_elements ht
      |> List.sort (fun (x,_) (y,_) -> compare x y)
    in
    hashtbl_update ht [(0, true); (1, false); (2, true)];
    assert_equal (elems ()) [(0, true); (1, false); (2, true)];
    hashtbl_update ht [];
    assert_equal (elems ()) [(0, true); (1, false); (2, true)];
    hashtbl_update ht [(5, true); (4, false)];
    assert_equal (elems ()) [(0, true); (1, false); (2, true); (4, false); (5, true)]
  );
  "hashtbl_from_list" >:: (fun _ ->
    let elems kv =
      Common.hashtbl_from_list kv
      |> Common.hashtbl_elements
      |> List.sort (fun (x,_) (y,_) -> compare x y)
    in
    let check_equal kv =
      assert_equal (elems kv) kv
    in
    check_equal [];
    check_equal [(0, true); (2, true)];
    check_equal [(0, true); (1, false); (2, true)];
  )
]

let _ = run_test_tt_main tests
