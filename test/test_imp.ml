open OUnit2
open Exp
open Imp
open Serialize

let assert_nexp (expected:nexp) (given:nexp) =
  let msg = "Expected: " ^ PPrint.n_to_s expected ^ "\nGiven: " ^ PPrint.n_to_s given in
  assert_equal expected given ~msg

let var x = Var (var_make x)

let tests = "test_predicates" >::: [
  "imp_to_post_1" >:: (fun _ ->
    (*
        local id = 32 + id;
        rw s_Q[id];
      *)
    let id = var_make "id" in
    let sq = var_make "s_Q" in
    let mk_acc e : Exp.access = {access_index = [Var e]; access_mode = W} in
    let wr = Acc (sq, mk_acc id) in
    let inc (x:variable) = Decl [(x, Local, Some (n_plus (Num 32) (Var x)))] in
    let p = Block [
      inc id;
      wr;
    ] in
    (* Translate: *)
    let p : Post.prog = p |> imp_to_post in
    (* Test: *)
    let open Imp.Post in
    (match p with
    | [
        Decl (_, Local, Some e1,[ (* local id = 32 + id; *)
          Acc (_, {access_index=[e2]; _}) (* rw s_Q[id]; *)
        ])
      ]
      ->
      assert_nexp (n_plus (Num 32) (Var id)) e1;
      assert_nexp (Var id) e2;
      ()
    | _ -> assert false
    );
    ()
  );
  "imp_to_post_2" >:: (fun _ ->
    (*
        local id = 32 + id in
        rw s_Q[id]
      *)
    let id = var_make "id" in
    let sq = var_make "s_Q" in
    let p : Post.prog = [
      let open Post in
      Decl (id, Local, Some (n_plus (Num 32) (Var id)),[ (* local id = 32 + id; *)
        Acc (sq, {access_index=[Var id]; access_mode = W}) (* rw s_Q[id]; *)
      ])
    ] in
    let p : Post.prog = Post.inline_decls p in
    match p with
    | [Post.Acc (_, {access_index=[e]; access_mode = W}) (* rw s_Q[32 + id]; *)
      ] ->
      assert_nexp (n_plus (Num 32) (Var id)) e;
      ()
    | _ -> assert false
  );
  "example1_a_post" >:: (fun _ ->
    (*
        local threadIdx.x;
        local id = threadIdx.x;
        rw s_Q[id];
        local id = 32 + id;
        rw s_Q[id];
      *)
    let id = var_make "id" in
    let tid = var_make "threadIdx.x" in
    let sq = var_make "s_Q" in
    let mk_acc e : Exp.access = {access_index = [Var e]; access_mode = W} in
    let wr = Acc (sq, mk_acc id) in
    let inc (x:variable) = Decl [(x, Local, Some (n_plus (Num 32) (Var x)))] in
    let p = Block [
      Decl[(tid, Local, None)];
      Decl [(id, Local, Some (Var tid))];
      wr;
      inc id;
      wr;
    ] in
    (* Translate: *)
    let p : Post.prog = p |> imp_to_post in
    (* Test: *)
    (match p with
    | [
       Post.(
        Decl (v1, Local, None,[ (*  local threadIdx.x; *)
          Decl (v2, Local, Some v2_e, (* local id = threadIdx.x; *)
            [
              Acc (_, {access_index=[e1]; _}); (* rw s_Q[id]; *)
              Decl (v3, Local, Some v3_e, [(* local id = 32 + id; *)
                Acc (_, {access_index=[e2]; _}) (* rw s_Q[id]; *)
              ])
            ]
          )
        ]))
      ] when v1 = tid && v2 = id && v3 = id
      ->
      let inc e = n_plus (Num 32) e in
      let id = Var id in
      assert_nexp id e1;
      assert_nexp (Var tid) v2_e;
      assert_nexp id e2;
      assert_nexp (inc id) v3_e;
      ()
    | _ -> assert false
    );
    ()
  );
  "example1_a" >:: (fun _ ->
    (*
        local threadIdx.x;
        local id = threadIdx.x;
        rw s_Q[id];
        local id = 32 + id;
        rw s_Q[id];
      *)
    let id = var_make "id" in
    let tid = var_make "threadIdx.x" in
    let wr = Acc (var_make "s_Q", {access_index = [Var id]; access_mode = W}) in
    let inc (x:variable) = Decl [(x, Local, Some (n_plus (Num 32) (Var x)))] in
    let p = Block [
      Decl[(tid, Local, None)];
      Decl [(id, Local, Some (Var tid))];
      wr;
      inc id;
      wr;
    ] in
    (* Translate: *)
    let p : Proto.prog = p
      |> imp_to_post
      |> Post.inline_decls
      |> Post.vars_distinct VarSet.empty
      |> post_to_proto
    in
    (* Test: *)
    begin
      let open Proto in
      match p with
      | [
          Acc (_, {access_index=[e1]; _});
          Acc (_, {access_index=[e2]; _})] ->
        let inc e = n_plus (Num 32) e in
        let tid = Var tid in
        assert_nexp tid e1;
        assert_nexp (inc tid) e2;
        ()
      | _ -> assert false
    end;
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
    let inc (x:variable) = Decl [(x, Local, Some (n_plus (Num 32) (Var x)))] in
    let p = Block [
      Decl[(var_make "threadIdx.x", Local, None)];
      Decl[(n, Local, Some (Var (var_make "threadIdx.x")))];
      Decl[(k, Local, Some (Var (var_make "blockIdx.x")))];
      Decl[(m, Local, Some (
        n_plus (Var n) (n_mult (Num 96) (Var k))
      ))];
      Decl [(id, Local, Some (Var n))];
      wr;
      inc id;
      wr;
      inc id;
      wr;
      inc id;
    ] in
    (* Translate: *)
    let p : Proto.prog = p
      |> imp_to_post
      |> Post.inline_decls
      |> Post.vars_distinct VarSet.empty
      |> post_to_proto
    in
    (* Test: *)
    begin
      let open Proto in
      match p with
      | [
          Acc (_, {access_index=[e1]; _});
          Acc (_, {access_index=[e2]; _});
          Acc (_, {access_index=[e3]; _})] ->
        let tid = Var (var_make "threadIdx.x") in
        let inc e = n_plus (Num 32) e in
        assert_nexp tid e1;
        assert_nexp (inc tid) e2;
        assert_nexp (inc (inc tid)) e3;
        ()
      | _ -> assert false
    end;
    ()

  );
]

let _ = run_test_tt_main tests
