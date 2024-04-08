open Protocols

open OUnit2
open Exp
open Imp

let assert_var (expected:Variable.t) (given:Variable.t)  =
  let msg = "Expected: " ^ Variable.name expected ^ "\nGiven: " ^ Variable.name given in
  assert_equal expected given ~msg

let assert_nexp (expected:nexp) (given:nexp) =
  let msg = "Expected: " ^ Exp.n_to_string expected ^ "\nGiven: " ^ Exp.n_to_string given in
  assert_equal expected given ~msg

let assert_post (expected:Scoped.t) (given:Scoped.t) =
  let msg =
    "Expected:\n" ^ Scoped.to_string expected ^
    "\nGiven:\n" ^ Scoped.to_string given
  in
  assert_equal expected given ~msg

let assert_ea (expected:Encode_assigns.t) (given:Encode_assigns.t) =
  let msg =
    "Expected:\n" ^ Encode_assigns.to_string expected ^
    "\nGiven:\n" ^ Encode_assigns.to_string given
  in
  assert_equal expected given ~msg

let tests = "test_predicates" >::: [
  "imp_to_post_1" >:: (fun _ ->
    (*
        local id = 32 + id;
        rw s_Q[id];
      *)
    let id = Variable.from_name "id" in
    let sq = Variable.from_name "s_Q" in
    let wr = Imp.Stmt.(Write {array=sq; index=[Var id]; payload=None}) in
    let inc (x:Variable.t) = Imp.Stmt.Decl [Decl.set x (n_plus (Num 32) (Var x))] in
    let p = Imp.Stmt.Block [
      inc id;
      wr;
    ] in
    (* Translate: *)
    let (_, p) = Scoped.from_stmt (Params.empty, p) in
    (* Test: *)
    let open Imp.Scoped in
    (match p with
    | Decl ({init=Some e1; _}, (* local id = 32 + id; *)
        Seq (Acc (_, {index=[e2]; _}), Skip) (* rw s_Q[id]; *)
      )
      ->
      assert_nexp (n_plus (Num 32) (Var id)) e1;
      assert_nexp (Var id) e2;
      ()
    | _ -> assert_failure (Scoped.to_string p)
    );
    ()
  );
  "imp_to_post_2" >:: (fun _ ->
    (*
        local id = 32 + id in
        rw s_Q[id]
      *)
    let id = Variable.from_name "id" in
    let sq = Variable.from_name "s_Q" in
    let p : Scoped.t =
      Decl (Decl.set id (n_plus (Num 32) (Var id)), (* local id = 32 + id; *)
        Acc (sq, {index=[Var id]; mode = Write None}) (* rw s_Q[id]; *)
      )
    in
    let p =
      Encode_assigns.from_scoped Variable.Set.empty p in
    match p with
    | Acc (_, {index=[e]; mode = Write None}) (* rw s_Q[32 + id]; *)
      ->
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
    let id = Variable.from_name "id" in
    let tid = Variable.from_name "threadIdx.x" in
    let sq = Variable.from_name "s_Q" in
    let wr = Imp.Stmt.(Write {array=sq; index=[Var id]; payload=None}) in
    let inc (x:Variable.t) =
      Imp.Stmt.Decl [Decl.set x (n_plus (Num 32) (Var x))]
    in
    let p = Imp.Stmt.(Block [
      Decl [Decl.unset tid];
      Decl [Decl.set id (Var tid)];
      wr;
      inc id;
      wr;
    ]) in
    (* Translate: *)
    let (_, p) = Scoped.from_stmt (Params.empty, p) in
    (* Test: *)
    (match p with
    | Scoped.(
        Decl ({var=v1; init=None; _}, (*  local threadIdx.x; *)
          Decl ({var=v2; init=Some v2_e; _}, (* local id = threadIdx.x; *)
            Seq (
              Acc (_, {index=[e1]; _}), (* rw s_Q[id]; *)
              Decl ({var=v3; init=Some v3_e; _}, (* local id = 32 + id; *)
                Seq (
                  Acc (_, {index=[e2]; _}), (* rw s_Q[id]; *)
                  Skip
                )
              )
            )
          )
        )
      )
      ->
      assert_var tid v1;
      assert_var id v2;
      assert_var id v3;
      let inc e = n_plus (Num 32) e in
      let id = Var id in
      assert_nexp id e1;
      assert_nexp (Var tid) v2_e;
      assert_nexp id e2;
      assert_nexp (inc id) v3_e;
      ()
    | _ -> assert_failure (Scoped.to_string p)
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
    let id = Variable.from_name "id" in
    let tid = Variable.from_name "threadIdx.x" in
    let sq = Variable.from_name "s_Q" in
    let wr = Imp.Stmt.(Write {array=sq; index=[Var id]; payload=None}) in
    let inc (x:Variable.t) =
      Imp.Stmt.Decl [Decl.set x (n_plus (Num 32) (Var x))]
    in
    let p = Imp.Stmt.(Block [
      Decl [Decl.unset tid];
      Decl [Decl.set id (Var tid)];
      wr;
      inc id;
      wr;
    ]) in
    (* Translate: *)
    let (_, p) = Scoped.from_stmt (Params.empty, p) in
    let p : Proto.Code.t = p
      |> Encode_assigns.from_scoped Variable.Set.empty
      |> Encode_asserts.from_encode_assigns
      |> Encode_asserts.to_proto
    in
    (* Test: *)
    begin
      match p with
      | Decl {body=Seq (
          Acc (_, {index=[e1]; _}),
          Acc (_, {index=[e2]; _})); _} ->
        let inc e = n_plus (Num 32) e in
        let tid = Var tid in
        assert_nexp tid e1;
        assert_nexp (inc tid) e2;
        ()
      | _ -> assert_failure (Proto.Code.to_string p)
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
    let n = Variable.from_name "n" in
    let m = Variable.from_name "m" in
    let k = Variable.from_name "k" in
    let id = Variable.from_name "id" in
    let sq = Variable.from_name "s_Q" in
    let wr = Imp.Stmt.(Write {array=sq; index=[Var id]; payload=None}) in
    let inc (x:Variable.t) =
      Imp.Stmt.Decl [Decl.set x (n_plus (Num 32) (Var x))]
    in
    let p = Imp.Stmt.(Block [
      Decl [Decl.unset (Variable.from_name "threadIdx.x")];
      Decl [Decl.set n (Var (Variable.from_name "threadIdx.x"))];
      Decl [Decl.set k (Var (Variable.from_name "blockIdx.x"))];
      Decl [Decl.set m (
        n_plus (Var n) (n_mult (Num 96) (Var k))
      )];
      Decl [Decl.set id (Var n)];
      wr;
      inc id;
      wr;
      inc id;
      wr;
      inc id;
    ]) in
    (* Translate: *)
    let (_, p) = Scoped.from_stmt (Params.empty, p) in
    let p : Proto.Code.t = p
      |> Encode_assigns.from_scoped Variable.Set.empty
      |> Encode_asserts.from_encode_assigns
      |> Encode_asserts.to_proto
    in
    (* Test: *)
    begin
      match p with
      | Decl {var=y; body=Seq (
          Acc (_, {index=[e1]; _}),
          Seq (
          Acc (_, {index=[e2]; _}),
          Acc (_, {index=[e3]; _}))); _} when Variable.name y = "threadIdx.x" ->
        let tid = Var (Variable.from_name "threadIdx.x") in
        let inc e = Bin (Plus, Num 32, e) in
        assert_nexp tid e1;
        assert_nexp (inc tid) e2;
        assert_nexp (inc (inc tid)) e3;
        ()
      | _ -> assert_failure (Proto.Code.to_string p)
    end;
    ()

  );
  "example3" >:: (fun _ ->
    let x = Variable.from_name "x" in
    let a = Variable.from_name "a" in
    let b = Variable.from_name "b" in
    let wr = Imp.Stmt.(Write {
      array=Variable.from_name "A";
      index=[Var a; Var b]; payload=None}
    ) in
    let p = Imp.Stmt.(Block [
      Decl [
        Decl.unset x;
        Decl.set a (Var x);
      ];
      Decl [
        Decl.unset x;
        Decl.set b (Var x);
      ];
      wr
    ]) in
    let p1 : Scoped.t =
      let open Scoped in
      (
        Decl (Decl.unset x,
          Decl (Decl.set a (Var x),
            Decl (Decl.unset x,
              Decl (Decl.set b (Var x),
                Seq (Acc (Variable.from_name "A", Access.write [Var a; Var b] None), Skip)
              )
            )
          )
        )
      )
    in
    assert_post (Scoped.from_stmt (Params.empty, p) |> snd) p1;
    let p2 : Scoped.t =
      let open Scoped in
      (*
        var x;
        var a = x;
        var x;
        var b = x;
        wr [a; b]
       *)
      Decl (Decl.unset x,
        Decl (Decl.set a (Var x),
          Decl (Decl.unset x,
            Decl (Decl.set b (Var x),
              Acc (Variable.from_name "A", Access.write [Var a; Var b] None)
            )
          )
        )
      )
    in
    let x1 = Variable.from_name "x1" in
    let p3 = 
      let open Encode_assigns in
      decl x (
        decl x1 (
          Acc (Variable.from_name "A", Access.write [Var x; Var x1] None)
        )
      )
    in
    assert_ea p3 (Encode_assigns.from_scoped Variable.Set.empty p2);
    (* Translate: *)
    let (_, p) = Scoped.from_stmt (Params.empty, p) in
    let p : Proto.Code.t = p
      |> Encode_assigns.from_scoped Variable.Set.empty
      |> Encode_asserts.from_encode_assigns
      |> Encode_asserts.to_proto
    in
    match p with
    | Decl {var=y1;
        body=Decl {var=y2; body=Acc (_, {index=[x1; x2]; _}); _}; _}
        when Variable.name y1 = "x" && Variable.name y2 = "x1" ->
      assert_nexp (Var (Variable.from_name "x")) x1;
      assert_nexp (Var (Variable.from_name "x1")) x2;
    | _ -> assert_failure (Proto.Code.to_string p)
  )
]

let _ = run_test_tt_main tests
