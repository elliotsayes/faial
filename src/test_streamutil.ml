open OUnit2
open Streamutil

let list_str (f:'a -> string) (l:'a list) : string =
  let x = List.map f l in
  match x with
  | [] -> "[]"
  | _ -> "[ " ^ (Common.join "," x) ^ " ]"

let stream_eq (f:'a->string) (l:'a list) (s:'a Stream.t) : unit =
    let g = stream_to_list s in
    let msg = "expected: " ^ (list_str f l)^ " given: " ^ (list_str f g) in
    assert_equal l g ~msg

let stream_int_eq = stream_eq string_of_int

let tests = "streams" >::: [
  "stream_to_list" >:: (fun _ ->
    let l = [1; 2; 3] in
    stream_int_eq l (Stream.of_list l);
    stream_int_eq [] (Stream.of_list [])
  );
  "stream_make" >:: (fun _ ->
    stream_int_eq [] (stream_make None)
  );
  "stream_seq" >:: (fun _ ->
    let s1 = Stream.of_list [1; 2; 3] in
    let s2 = Stream.of_list [4; 5; 6] in
    let e = [1; 2; 3; 4; 5; 6] in
    let g = stream_seq s1 s2 |> stream_to_list in
    assert_equal e g
  );
  "stream_take" >:: (fun _ ->
    stream_int_eq [1;2;3] (
      Stream.of_list [1;2;3;4] |>
      stream_take 3 |>
      Stream.of_list
    )
  )
]

let _ = run_test_tt_main tests
