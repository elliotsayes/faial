open OUnit2
open Stage0
open Protocols
open Inference
open Queries
open C_lang

module VarSet = Variable.Set

let parm_var_decl ?(ty=C_type.j_int_type) (name:string) : Expr.t =
  ParmVarDecl {name=Variable.from_name name; ty=ty}

let tests = "tests" >::: [

  "variables" >:: (fun _ ->
    let open C_lang.Expr in
    let assert_vars expected given =
      let expected =
        expected
        |> List.map Variable.from_name
        |> VarSet.of_list
      in
      let given = Variables.from_expr given |> Variables.to_set in
      let to_s (x:VarSet.t) =
        VarSet.elements x
        |> List.map Variable.name
        |> Common.join ", "
      in
      assert_equal
        ~printer:(fun x -> "[" ^ to_s x ^ "]")
        expected
        given
    in
    BinaryOperator {
        opcode="+";
        lhs=parm_var_decl "x";
        rhs=IntegerLiteral 0;
        ty=C_type.j_int_type;
    } |> assert_vars ["x"];
    BinaryOperator {
        opcode="+";
        lhs=IntegerLiteral 0;
        rhs=IntegerLiteral 0;
        ty=C_type.j_int_type;
    } |> assert_vars [];
    BinaryOperator {
        opcode="+";
        lhs=parm_var_decl "x";
        rhs=parm_var_decl "y";
        ty=C_type.j_int_type;
    } |> assert_vars ["x"; "y"];
  );
  "Loops.make" >:: (fun _ ->
    let open Loops in
    let assert_make expected given =
      assert_equal
        ~printer:to_string
        expected (make given)
    in
    let g_for ?(body=[]) idx = Stmt.ForStmt {
      init=None;
      cond=Some (IntegerLiteral idx);
      inc=None;
      body=CompoundStmt body
    } in
    let e_for ?(body=[]) ?(data=[]) idx =
      For {
        init=None;
        cond=Some (IntegerLiteral idx);
        inc=None;
        data=CompoundStmt data;
        body=body
      }
    in
    assert_make [e_for 0] (g_for 0);
    assert_make [
      e_for 0 ~body:[
        e_for 1
      ] ~data:[
        ReturnStmt;
        g_for 1 ~body:[]
      ]
    ] (
      g_for ~body:[
        ReturnStmt;
        g_for 1 ~body:[]
      ] 0
    )
  );
  "Loops.filter_using_loop_vars" >:: (fun _ ->
    let open Loops in
    let given = [
      For {
        init=None;
        cond=None;
        inc=None;
        data=ReturnStmt;
        body=[]
      }
    ]
    in
    assert_equal [] (filter_using_loop_vars given)
  )
]


let _ = run_test_tt_main tests
