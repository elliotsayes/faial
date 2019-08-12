open OUnit2
open Common

let tests = "ends_with" >::: [
  "ends_with" >:: (fun _ ->
      assert_bool "" (ends_with "foo" "");
      assert_bool "" (ends_with "foo" "o");
      assert_bool "" (ends_with "foo" "oo");
      assert_bool "" (ends_with "foo" "foo");
      assert_bool "" (not (ends_with "foo" "moo"))
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
