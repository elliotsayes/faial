open OUnit2
open Inference
open D_lang
open Protocols
open C_type

let make_int ?(ty=(mk_j_type "int")) (var:string) =
  Ty_variable.make
    ~name:(Variable.from_name var)
    ~ty:ty

let mk_var ?(ty="int") x =
  Expr.Ident {name=Variable.from_name x; kind=Var; ty=mk_j_type ty}

(*let make_decl ?(assignment=0) ?(ty="int") ?(var_name="x") () =
  let ty_var = make_int ~ty:(mk_j_type ty) var_name in
  let init = Init.IExpr(D_lang.Expr.IntegerLiteral assignment) in
  Decl.from_init ty_var init 
*)

let make_init ?(assignment=0) ?(ty="int") ?(var_name="x") () =  
  let ty_var = make_int ~ty:(mk_j_type ty) var_name in
  let init = Init.IExpr(D_lang.Expr.IntegerLiteral assignment) in
  let decl = Decl.from_init ty_var init in
  ForInit.Decls(decl :: [])   

let make_cond ?(op="<") ?(lhs="x") rhs =
  let lhs = mk_var lhs in
  let rhs = Expr.IntegerLiteral rhs in
  Expr.BinaryOperator{lhs = lhs;
                      rhs = rhs;
                      opcode = op;
                      ty = `Null}


let make_op (op:string) (lhs:Expr.t) (rhs:Expr.t) =
  Expr.BinaryOperator{
    lhs = lhs;
    rhs = rhs;
    opcode = op;
    ty = `Null
  }
  
  
let make_operation op lhs rhs =
  let lhs = mk_var lhs in
  Expr.BinaryOperator{
    lhs = lhs;
    rhs = rhs;
    opcode = op;
    ty = `Null}
    
let make_operation_int op lhs rhs =
  let lhs = mk_var lhs in
  let rhs = Expr.IntegerLiteral rhs in
  Expr.BinaryOperator{
    lhs = lhs;
    rhs = rhs;
    opcode = op;
    ty = `Null}
    
let iteration l r opcode =
  Expr.BinaryOperator
    {
      lhs = l;
      opcode = "=";
      rhs =
        Expr.BinaryOperator
          {
            lhs = l;
            rhs = r;
            opcode = opcode;
            ty = `Null
          };
      ty = `Null
    }


let make_iteration ?(opcode="+") ?(var_name="x") ?(change=1) () =
  let l = mk_var var_name in
  let r = Expr.IntegerLiteral change in
  iteration l r opcode
  

let for_from_parts ?(init=None) ?(cond=None) ?(inc=None) () =
  {      Stmt.init = init;
      Stmt.cond = cond;
      Stmt.inc = inc;
      Stmt.body = Stmt.ContinueStmt;
    }
let while_from_parts cond body = {Stmt.cond = cond; Stmt.body = body}

let tests = "loops" >::: [
    let open Loop_infer in
    "for_loop" >:: (fun _ ->       
        (* for (x = 0; x < 100; x = x+1) *)        
        let parse_loop =
           for_from_parts
            ~init:(Some (make_init ()))
            ~cond:(Some (make_cond 100))
            ~inc:(Some (make_iteration ())) () |>
           from_for in
        assert(parse_loop =
               Some{
                 init = Expr.IntegerLiteral 0;
                 name = Variable.from_name "x";
                 cond =
                   {
                     op = Lt;
                     arg = D_lang.Expr.IntegerLiteral 100;
                   };
                 inc =
                   {
                     op = Plus;
                     arg = Expr.IntegerLiteral 1;
                   }
               });
        (*while(x < 100){x = x + 1}*)
        let cond = (make_operation_int "<" "x" 100) in
        let last = (make_operation "=" "x" (make_operation_int "+" "x" 1)) in
        let compound = Stmt.CompoundStmt([Stmt.SExpr(last)]) in
        let while_loop = while_from_parts cond compound in
        let expected = Some({
            name = Variable.from_name "x";
            init = mk_var "x";
            cond = {
              op = Lt;
              arg = D_lang.Expr.IntegerLiteral 100;
            };
            inc = {
              op = Plus;
              arg = Expr.IntegerLiteral 1;
            };
          },Stmt.CompoundStmt([])) in
        assert(from_while(while_loop) = expected);

        (*while(x < 100) {my_sum = x + 30; x = x + 1;}*)
        let cond = (make_operation_int "<" "x" 100) in
        let last = (make_operation "=" "x" (make_operation_int "+" "x" 1)) in
        let stmt1 = (make_operation "=" "my_sum" (make_operation_int "+" "x" 30)) in
        let compound = Stmt.CompoundStmt([Stmt.SExpr(stmt1);Stmt.SExpr(last)]) in
        let while_loop = while_from_parts cond compound in
        let expected = Some({
            name = Variable.from_name "x";
            init = mk_var "x";
            cond = {
              op = Lt;
              arg = D_lang.Expr.IntegerLiteral 100;
            };
            inc = {
              op = Plus;
              arg = Expr.IntegerLiteral 1;
            };
          },Stmt.CompoundStmt([Stmt.SExpr(stmt1)])) in
        assert(from_while(while_loop) = expected);

        (*while(x < 100) {my_sum = x + 30.0; x = x + 2.5;}*)

        let cond_arg = mk_var ~ty:"float" "x" in
        let cond = (make_op "<"
                      cond_arg
                      (Expr.FloatingLiteral(100.0))) in
        let last = (make_operation "=" "x" (make_op "+" cond_arg (Expr.FloatingLiteral(2.5))))  in
        let stmt1 = (make_operation "=" "my_sum" (make_op "+" cond_arg (Expr.FloatingLiteral(30.0)))) in
        let compound = Stmt.CompoundStmt([Stmt.SExpr(stmt1);Stmt.SExpr(last)]) in
        let while_loop = while_from_parts cond compound in
        let expected = Some({
            name = Variable.from_name "x";
            init = mk_var "x";
            cond = {
              op = Lt;
              arg = Expr.FloatingLiteral 100.0;
            };
            inc = {
              op = Plus;
              arg = Expr.FloatingLiteral 2.5;
            };
          },Stmt.CompoundStmt([Stmt.SExpr(stmt1)])) in
        assert(from_while(while_loop) = expected);
        
        (*
        (* for(x = 0; x<= 100; x = x+1) *)
        let parse_loop =
           for_from_parts
            ~init:(Some (make_init ()))
            ~cond:(Some (make_cond ~op:"<=" 100))
            ~inc:(Some (make_iteration ())) () |>
           parse_for in
        assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 0);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.LtEq;
                     Loops.arg = D_lang.Expr.IntegerLiteral 100;
                   };
                 inc =
                   {
                     Loops.op = Loops.Plus;
                     Loops.arg = Expr.IntegerLiteral 1;
                   }
               });


        (* for(x = -4; x<= 100; x = x+1) *)
        
        let parse_loop =
           for_from_parts
            ~init:(Some (make_init ~assignment:(-4) ()))
            ~cond:(Some (make_cond ~op:"<=" 100))
            ~inc:(Some (make_iteration ())) () |>
           parse_for in
        assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral (-4));
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.LtEq;
                     Loops.arg = D_lang.Expr.IntegerLiteral 100;
                   };
                 inc =
                   {
                     Loops.op = Loops.Plus;
                     Loops.arg = Expr.IntegerLiteral 1;
                   }
               });


        (* for(x = 0; x< 100; x = x+2) *)
        
        let parse_loop =
           for_from_parts
            ~init:(Some (make_init ()))
            ~cond:(Some (make_cond 100))
            ~inc:(Some (make_iteration ~change:2 ())) () |>
           parse_for in
        assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 0);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.Lt;
                     Loops.arg = D_lang.Expr.IntegerLiteral 100;
                   };
                 inc =
                   {
                     Loops.op = Loops.Plus;
                     Loops.arg = Expr.IntegerLiteral 2;
                   }
               });

        (* for(x = 100; x>0; x =x-3) *)

        let parse_loop =
           for_from_parts
            ~init:(Some (make_init ~assignment:100 ()))
            ~cond:(Some (make_cond ~op:">" 0))
            ~inc:(Some (make_iteration ~change:3 ~opcode:"-" ())) () |>
           parse_for in
                assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 100);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.Gt;
                     Loops.arg = D_lang.Expr.IntegerLiteral 0;
                   };
                 inc =
                   {
                     Loops.op = Loops.Minus;
                     Loops.arg = Expr.IntegerLiteral 3;
                   }
               });

        (* for(x = 100; x>0; x = x/3) *)
         let parse_loop =
           for_from_parts
            ~init:(Some (make_init ~assignment:100 ()))
            ~cond:(Some (make_cond ~op:">" 0))
            ~inc:(Some (make_iteration ~change:3 ~opcode:"/" ())) () |>
           parse_for in 
        assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 100);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.Gt;
                     Loops.arg = D_lang.Expr.IntegerLiteral 0;
                   };
                 inc =
                   {
                     Loops.op = Loops.Div;
                     Loops.arg = Expr.IntegerLiteral 3;
                   }
               });

        (* for(x = 100; x>0; x = x>>2) *)
         let parse_loop =
           for_from_parts
            ~init:(Some (make_init ~assignment:100 ()))
            ~cond:(Some (make_cond ~op:">" 0))
            ~inc:(Some (make_iteration ~change:2 ~opcode:">>" ())) () |>
           parse_for in
         assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 100);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.Gt;
                     Loops.arg = D_lang.Expr.IntegerLiteral 0;
                   };
                 inc =
                   {
                     Loops.op = Loops.RShift;
                     Loops.arg = Expr.IntegerLiteral 2;
                   }
               });
            (* for(x = 0 ;x <100; x=x+1,y=y+1) *)         
         let parse_loop =
           for_from_parts
             ~init:(Some (make_init ()))
             ~inc:(Some (BinaryOperator
                           {
                             opcode=",";
                             lhs=make_iteration ~change:1 ~opcode:"+" ();
                             rhs=make_iteration ~change:1 ~opcode:"+" ~var_name:"y" ();
                             ty = `Null;
                           }))
             ~cond:(Some (make_cond ~op:"<" 100)) () |>
           parse_for in 
         assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 0);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.Lt;
                     Loops.arg = D_lang.Expr.IntegerLiteral 100;
                   };
                 inc =
                   {
                     Loops.op = Loops.Plus;
                     Loops.arg = Expr.IntegerLiteral 1;
                   }
               });         

        (* for(x = 0; x<=100; x = x<<3) *)
         let parse_loop =
           for_from_parts
            ~init:(Some (make_init ()))
            ~cond:(Some (make_cond ~op:"<=" 100))
            ~inc:(Some (make_iteration ~change:3 ~opcode:"<<" ())) () |>
           parse_for in
         
        assert(parse_loop =
               Some{
                 init = (Expr.IntegerLiteral 0);
                 name = Variable.from_name "x";
                 cond =
                   {
                     Loops.op = Loops.LtEq;
                     Loops.arg = D_lang.Expr.IntegerLiteral 100;
                   };
                 inc =
                   {
                     Loops.op = Loops.LShift;
                     Loops.arg = Expr.IntegerLiteral 3;
                   }
               });

        (* for(x = 0; x<=100) *)
         let parse_loop =
           for_from_parts
            ~init:(Some (make_init ()))
            ~cond:(Some (make_cond ~op:"<=" 100)) () |>
           parse_for in assert(parse_loop = None);

         (* for(;x > 0; x--) *)
         
         let parse_loop =
           for_from_parts
             ~inc:(Some (make_iteration ~change:1 ~opcode:"-" ()))
             ~cond:(Some (make_cond ~op:">" 0)) () |>
           parse_for in assert(parse_loop = None);

         (* for(;x > 0; x--) *)
         
         let parse_loop =
           for_from_parts
             ~inc:(Some (make_iteration ~change:1 ~opcode:"-" ()))
             ~cond:(Some (make_cond ~op:">" 0)) () |>
           parse_for in assert(parse_loop = None);         
      
      );

    "for_init_fix" >:: (fun _ ->
        
          (*{ 
           int x = 0; 
           for(; x < 100; x++) 
           }
          *)
         let loop_no_init = For.empty
                    |> For.set_cond (make_cond 100)
                    |> For.set_inc (make_iteration ()) in
         let split_decl = [make_decl()]  in
         let united_loop = loop_no_init |>
                           For.set_init (ForInit.Decls(split_decl)) in
         let compound_stmt = [CompoundStmt([DeclStmt(split_decl);loop_no_init]);] in
         assert(for_init_fix(compound_stmt) = [Stmt.CompoundStmt([united_loop])]);
         
         (*
         int x = 0;
         for(; x < 100; x++) *)
                
         let stmt_list = [DeclStmt(split_decl);loop_no_init] in
         assert(for_init_fix(stmt_list) = [united_loop]);

         (*{ 
           int x = 0; 
           for(; x < 100; x++) 
           }
           int x = 0;
           for(; x < 100; x++)
         *)
       
         let stmt_list = [DeclStmt(split_decl);loop_no_init] in
         assert(for_init_fix(CompoundStmt(stmt_list) :: stmt_list) = [CompoundStmt([united_loop]); united_loop]);

         (* 
           int x = 0; 
           for(; x < 100; x++) 
           int y = 0; 
           for(; y < 100; x++) 
           *)
        
      
         let stmt_list = [DeclStmt(split_decl);loop_no_init;DeclStmt(split_decl);loop_no_init;] in
         assert(for_init_fix(stmt_list) = [united_loop;united_loop]);

        *)
      );
            
  ]
let _ = run_test_tt_main tests
