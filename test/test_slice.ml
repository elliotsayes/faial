open OUnit2

let tests = "tests" >::: [

  "substring" >:: (fun _ ->
    [
      (1, Some (-1), "sd");
      (1, Some 0, "");
      (1, None, "sdf");
      (-1, None, "f");
      (-2, None, "df");
      (-4, None, "asdf");
      (1, Some (-4), "");
      (0, Some 0, "");
      (0, Some 1, "a");
    ]
    |> List.iter (fun (start, finish, expected) ->
      let f = match finish with
      | None -> "?"
      | Some n -> string_of_int n
      in
      let given = "asdf" in
      assert_equal
        ~msg:(Printf.sprintf "\"%s\"[%d:%s]" given start f)
        ~printer:(fun x -> "'" ^ x ^ "'")
        expected
        (Slice.make ~start ~finish |> Slice.string given)
    )
  );
]

let _ = run_test_tt_main tests
