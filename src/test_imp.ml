open OUnit2
open Exp
open Imp
open Serialize

let assert_kv (given:(variable * nexp) list) (expected:(variable * nexp) list): unit = 
  let kv_to_s l =
    l
    |> List.map (fun (k,v) -> var_name k ^ "=" ^ PPrint.n_to_s v)
    |> Common.join ", "
    |> fun n -> "[" ^ n ^ "]"
  in
  let sort_kv =
    List.sort (fun (x,_) (y,_) -> String.compare (var_name x) (var_name y))
  in
  let msg = "expected: "^ kv_to_s expected ^ " given: " ^ kv_to_s given in
  assert_equal (sort_kv given) (sort_kv expected) ~msg

let kv_s = List.map (fun (x,v) -> (var_make x, v))

let assert_kv_s (given:(string * nexp) list) (expected:(string * nexp) list) : unit =
  assert_kv (kv_s given) (kv_s expected)

let assert_norm (given:(string * nexp) list) (expected:(string * nexp) list) : unit =
  assert_kv (normalize_deps (kv_s given)) (kv_s expected)

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
  "example1" >:: (fun _ ->
    (*
        local threadIdx.x;
        local n = threadIdx.x;
        local k = blockIdx.x;
        local m = n + (96 * k);
        local id = n;
        rw s_Q[id];
        local m = 32 + m;
        local id = 32 + id;
        rw s_Q[id];
        local m = 32 + m;
        local id = 32 + id;
        rw s_Q[id];
      *)
    let n = var_make "n" in
    let m = var_make "m" in
    let k = var_make "k" in
    let id = var_make "id" in
    let wr = Acc (var_make "s_Q", {access_index = [Var id]; access_mode = W}) in
    let inc (x:variable) = Decl (x, Local, Some (n_plus (Num 32) (Var x))) in
    let p = Block [
      Decl(var_make "threadIdx.x", Local, None);
      Decl(n, Local, Some (Var (var_make "threadIdx.x")));
      Decl(k, Local, Some (Var (var_make "blockIdx.x")));
      Decl(m, Local, Some (
        n_plus (Var n) (n_mult (Num 96) (Var k))
      ));
      Decl(id, Local, Some (Var n));
      wr;
      inc id;
      wr;
      inc id;
      wr;
      inc id;
    ] in
    let vs = VarSet.of_list [var_make "gridDim.x"; var_make "blockIdx.x"; var_make "blockDim.x"] in
    vs |> PPrint.var_set_to_s |> print_endline;
    let p = normalize_variables p vs in
    stmt_to_s p |> PPrint.print_doc;
    let inc k (v:string) =
      var_make k, n_plus (Num 32) (Var (var_make v))
    in
    (*
      id2 = 32 + id1
      m2 = 32 + m1
      id1 = 32 + id
      m1 = 32 + m
      id = n
      m = n + (96 * k)
      k = blockIdx.x
      n = threadIdx.x
    *)    
    (* let expected_kvs = [
      inc "id2" "id1";
      inc "m2" "m1";
      inc "id1" "id";
      inc "m1" "m";
      id, Var n;
      m, n_plus (Var n) (n_mult (Num 96) (Var k));
      k, Var (var_make "blockIdx.x");
      n, Var (var_make "threadIdx.x");
    ] in
    *)
    let kvs = get_var_binders p [] in
    (* assert_kv expected_kvs kvs; *)
    let kvs = kvs |> normalize_deps in
    List.iter (fun (k,v) ->
      print_endline ((var_name k) ^ " = " ^ (Serialize.PPrint.n_to_s v));
    ) kvs;
    (*
      local threadIdx.x;
      local n = threadIdx.x;
      local k = blockIdx.x;
      local m = n + (96 * k);
      local id = n;
      rw s_Q[id];
      local m1 = 32 + m;
      local id1 = 32 + id;
      rw s_Q[id1];
      local m2 = 32 + m1;
      local id2 = 32 + id1;
      rw s_Q[id2];
    *)
    ()

  );
]

let _ = run_test_tt_main tests
