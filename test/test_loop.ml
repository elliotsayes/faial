open OUnit2
open Inference
open Protocols
open Loops    
open D_lang
open C_type


let make_int ?(ty=(mk_j_type "int")) var =
  TyVariable.make ~name:(Variable.from_name var) ~ty:ty

let make_decl ?(assignment=0) ?(ty="int") ?(var_name="x") () =
  let ty_var = make_int ~ty:(mk_j_type ty) var_name in
  let init = Init.IExpr(D_lang.Expr.IntegerLiteral assignment) in
  Decl.from_init ty_var init 

let make_init ?(assignment=0) ?(ty="int") ?(var_name="x") () =  
  let ty_var = make_int ~ty:(mk_j_type ty) var_name in
  let init = Init.IExpr(D_lang.Expr.IntegerLiteral assignment) in
  let decl = Decl.from_init ty_var init in
  ForInit.Decls(decl :: [])   

let make_cond ?(op="<") ?(lhs="x") rhs =
  let lhs = Expr.VarDecl (make_int lhs) in
  let rhs = Expr.IntegerLiteral rhs in
  Expr.BinaryOperator{lhs = lhs;
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
  let l = Expr.VarDecl(make_int var_name) in
  let r = Expr.IntegerLiteral change in
  iteration l r opcode
  

let for_from_parts ?(init=None) ?(cond=None) ?(inc=None) () =
    {
      Stmt.init = init;
      Stmt.cond = cond;
      Stmt.inc = inc;
      Stmt.body = Stmt.ContinueStmt;
    }

let tests = "loops" >::: [
    "for_loop" >:: (fun _ ->

        (* for (x = 0; x < 100; x = x+1) *)        
        let parse_loop =
           for_from_parts
            ~init:(Some (make_init ()))
            ~cond:(Some (make_cond 100))
            ~inc:(Some (make_iteration ())) () |>
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


         (*{ 
           int x = 0; 
           for(; x < 100; x++) 
           }
         *)
         
         let loop =
           Stmt.ForStmt(for_from_parts
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~op:"<" 100)) ()) in
         let init = Stmt.DeclStmt(make_decl() :: []) in
         let compound_stmt = Stmt.CompoundStmt([init;loop]) in
         assert(for_fix(compound_stmt :: []) =
                (Stmt.CompoundStmt(
                    (Stmt.ForStmt(
                        for_from_parts
                          ~init:(Some(make_init()))
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~op:"<" 100)) ())) :: []) :: []));
         (*{ 
           int x = 0; 
           for(; x < 100; x++) 
           }
           int x = 100;
           for(; x < 200; x++)
         *)
         
         let loop =Stmt.ForStmt(for_from_parts
                                  ~inc:(Some (make_iteration()))
                                  ~cond:(Some (make_cond ~op:"<" 100)) ()) in
         let init = Stmt.DeclStmt(make_decl() :: []) in
         let compound_stmt = Stmt.CompoundStmt([init;loop]) in
         let init2 = Stmt.DeclStmt(make_decl ~assignment:100 () :: []) in
         let loop2 = Stmt.ForStmt(for_from_parts
                                    ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                                    ~cond:(Some (make_cond ~op:"<" 200)) ()) in           
         assert(for_fix(compound_stmt :: init2 :: loop2 :: []) =
                (Stmt.CompoundStmt(
                    (Stmt.ForStmt(
                        for_from_parts
                          ~init:(Some(make_init()))
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond 100)) ())) :: [])) ::
                (Stmt.ForStmt(for_from_parts
                                ~init:(Some(make_init ~assignment:100 ()))
                                ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                                ~cond:(Some (make_cond ~op:"<" 200)) ())) 
                   :: []);

         (* 
           int x = 0; 
           for(; x < 100; x++) 
           *)
         
         let loop =
           Stmt.ForStmt(for_from_parts
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~op:"<" 100)) ()) in
         let init = Stmt.DeclStmt(make_decl() :: []) in
         assert(for_fix(init :: loop :: []) =
                    (Stmt.ForStmt(
                        for_from_parts
                          ~init:(Some(make_init()))
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~op:"<" 100)) ())) :: []);

          (* 
           int x = 0; 
           for(; x < 100; x++) 
           int y = 0; 
           for(; y < 100; x++) 
           *)
         
         let loop1 =
           Stmt.ForStmt(for_from_parts
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~op:"<" 100)) ()) in
         let loop2 =
           Stmt.ForStmt(for_from_parts
                          ~inc:(Some (make_iteration ~var_name:"y" ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~lhs:"y" ~op:"<" 100)) ()) in
         let init1 = Stmt.DeclStmt(make_decl() :: []) in
         let init2 = Stmt.DeclStmt(make_decl ~var_name:"y" () :: []) in
         assert(for_fix(init1 :: loop1 :: init2 :: loop2 :: []) =
                    (Stmt.ForStmt(
                        for_from_parts
                          ~init:(Some(make_init()))
                          ~inc:(Some (make_iteration ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~op:"<" 100)) ())) ::
                    (Stmt.ForStmt(
                        for_from_parts
                          ~init:(Some(make_init ~var_name:"y"()))
                          ~inc:(Some (make_iteration ~var_name:"y" ~change:1 ~opcode:"+" ()))
                          ~cond:(Some (make_cond ~lhs:"y" ~op:"<" 100)) ())) :: []);

         
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
      );
    


  ]
let _ = run_test_tt_main tests
