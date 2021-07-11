open OUnit2
open Exp
open Imp
open Serialize

let assert_norm (given:(string * nexp) list) (expected:(string * nexp) list) : unit =
  let as_var = List.map (fun (x,v) -> (var_make x, v)) in
  let given = as_var given in
  let expected = as_var expected in
  let kv_to_s l =
    l
    |> List.map (fun (k,v) -> var_name k ^ "=" ^ PPrint.n_to_s v)
    |> Common.join ", "
    |> fun n -> "[" ^ n ^ "]"
  in
  let sort_kv =
    List.sort (fun (x,_) (y,_) -> String.compare (var_name x) (var_name y))
  in
  let given = normalize_deps given |> sort_kv in
  let expected = expected |> sort_kv in
  let msg = "expected: "^ kv_to_s expected ^ " given: " ^ kv_to_s given in
  assert_equal given expected ~msg

let make_kv n (x,vs) =
  (x, List.fold_left n_plus (Num n) (List.map (fun x -> (Var (var_make x))) vs))

let make_kvs = List.mapi make_kv

let assert_norm_leafs (given:string list) : unit =
  let given = make_kvs (List.map (fun x-> (x, [])) given) in
  assert_norm given given

let var x = Var (var_make x)

let tests = "test_predicates" >::: [
  "leafs" >:: (fun _ ->
    assert_norm_leafs [];
    assert_norm_leafs ["x"];
    assert_norm_leafs ["x"; "y"];
    assert_norm_leafs ["x"; "y"; "z"];
    ()
  );
  "one level" >:: (fun _ ->
    assert_norm [
      "x", Num 1;
      "y", n_plus (Num 10) (Var (var_make "x"))
    ] [
      "x", Num 1;
      "y", Num 11;
    ];
    assert_norm [
      "x", Num 1;
      "y", n_plus (Num 10) (Var (var_make "x"));
      "z", n_plus (Var (var_make "x")) (Var (var_make "x"));
    ] [
      "x", Num 1;
      "y", Num 11;
      "z", Num 2;
    ];
    ()
  );
  "2 levels" >:: (fun _ ->
    assert_norm [
      "x", Num 1;
      "y", n_plus (Num 2) (var "x");
      "z", n_plus (Num 3) (var "y");
    ] [
      "x", Num 1;
      "y", Num 3;
      "z", Num 6;
    ];
    assert_norm [
      "x", Num 1;
      "z", n_plus (Num 3) (var "y");
      "y", n_plus (Num 2) (var "x");
    ] [
      "x", Num 1;
      "y", Num 3;
      "z", Num 6;
    ];
    assert_norm [
      "y", n_plus (Num 2) (var "x");
      "z", n_plus (Num 3) (var "y");
      "x", Num 1;
    ] [
      "x", Num 1;
      "y", Num 3;
      "z", Num 6;
    ];
    ()
  );
  "3 levels" >:: (fun _ ->
    (*
      x -> y ----|
       \  \-> z -|-> w
        ---------|
      *)
    assert_norm [
      "x", Num 1;
      "y", n_plus (Num 2) (Var (var_make "x"));
      "z", n_plus (Num 3) (Var (var_make "y"));
      "w", n_plus (Var (var_make "y")) (Var (var_make "z"));
    ] [
      "x", Num 1;
      "y", Num 3;
      "z", Num 6;
      "w", Num 9;
    ];
    assert_norm [
      "chunkIndex", n_mult (var "threadIndex") (var "chunkSize");
      "threadIndex", n_plus (var "treadIdx.x") (n_mult (var "blockDim.x") (var "blockIdx.x"));
      "hashIndex", n_plus (Num 4) (var "threadIndex");
    ] [
      "chunkIndex", n_mult (n_plus (var "treadIdx.x") (n_mult (var "blockDim.x") (var "blockIdx.x"))) (var "chunkSize");
      "threadIndex", n_plus (var "treadIdx.x") (n_mult (var "blockDim.x") (var "blockIdx.x"));
      "hashIndex", n_plus (Num 4) (n_plus (var "treadIdx.x") (n_mult (var "blockDim.x") (var "blockIdx.x")));
    ];
    ()
  );
]

let _ = run_test_tt_main tests
