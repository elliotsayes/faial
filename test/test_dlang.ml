open OUnit2
open Inference
open D_lang

let tests = "dlang" >::: [
    "stmt_last" >:: (fun _ ->
        let open Stmt in
        let compound_stmt = CompoundStmt[BreakStmt;GotoStmt;ContinueStmt] in
        assert(last(compound_stmt) = (CompoundStmt([BreakStmt;GotoStmt]),ContinueStmt));
      );  
]

let _ = run_test_tt_main tests
