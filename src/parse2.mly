%token <int> UINT
%token <string> ID
%token SEMICOLON PLUS MINUS MULT DIV MOD LT GT GTE LTE OR AND EQ NEQ
%token NOT
%token EOF
%token ONE TWO AT
%token LOCAL
%token SYNC RW RO IF
%token LOCS CONST ASSERT COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token FOREACH

%left OR
%left AND
(*
%left EQ NEQ
%left LT GT GTE LTE
*)
%left PLUS MINUS
%left MULT DIV MOD

%{

  open Proto
  type modifier = Local | Global

%}

%start <Proto.kernel> main
%%
main : p = kernel EOF { p };

num:
  | ONE { 1 }
  | TWO { 2 }
  | n = UINT { n }

nexp:
  | i = num { Num i }
  | x = ID { Var x }
  | LPAREN n = nexp RPAREN { n }
  | n1 = nexp ; o = nbin ; n2 = nexp { o n1 n2 }
  | ONE AT n = nexp { Proj(Task1, n) }
  | TWO AT n = nexp { Proj(Task2, n) }

%inline nbin:
  | PLUS { n_plus }
  | MINUS { n_minus }
  | MULT { n_mult }
  | DIV { n_div }
  | MOD { n_mod }

bexp:
  | LPAREN b = bexp RPAREN { b }
  | n1 = nexp; o = nrel; n2 = nexp { o n1 n2 }
  | b1 = bexp; o = brel; b2 = bexp { o b1 b2 }
  | NOT b = bexp { b_not b }
  | p = ID LPAREN x = ID RPAREN { Pred (p, x) }

%inline nrel:
  | EQ { n_eq }
  | NEQ { n_neq }
  | LT { n_lt }
  | GT { n_gt  }
  | LTE { n_le }
  | GTE { n_ge }

%inline brel:
  | OR { b_or }
  | AND { b_and }

mode: RW { W } | RO { R };

proto:
  | SYNC { Sync }
  | ASSERT b = bexp { Assert b }
  | p1 = proto SEMICOLON p2 = proto
    { Seq (p1, p2) }
  | m = mode x = ID LBRACK n = nexp RBRACK
    { Acc (x, {access_index=n; access_cond=Bool true; access_mode=m}) }
  | m = mode x = ID LBRACK n = nexp RBRACK IF b = bexp
    { Acc (x, {access_index=n; access_cond=b; access_mode=m}) }
  | FOREACH x = ID LT n = nexp LBRACE p = proto RBRACE
    { Loop ({range_var=x; range_upper_bound=n}, p) }
  | p = proto SEMICOLON { p }

loc_names:
  | { [] }
  | x = ID { [x] }
  | x = ID COMMA xs = loc_names { x :: xs }

var_names:
  | { [] }
  | x = ID { [Global, x] }
  | LOCAL x = ID { [Local, x] }
  | x = ID COMMA xs = var_names { (Global, x):: xs }
  | LOCAL x = ID COMMA xs = var_names { (Local, x) :: xs }

locs:
  | LOCS l1 = loc_names SEMICOLON { l1 }

const:
  | CONST l2 = var_names SEMICOLON { l2 }

kernel:
  | l1 = locs
    l2 = loption(const)
    p = proto {
      let ls, gs = List.partition (fun (x,_) -> x = Local) l2 in
      let ls = List.map snd ls in
      let gs = List.map snd gs in
      {
        kernel_locations = l1;
        kernel_local_variables = ls;
        kernel_global_variables = gs;
        kernel_code = p;
      }
    }
  | l2 = loption(const)
    l1 = locs
    p = proto {
      let ls, gs = List.partition (fun (x,_) -> x = Local) l2 in
      let ls = List.map snd ls in
      let gs = List.map snd gs in
      {
        kernel_locations = l1;
        kernel_local_variables = ls;
        kernel_global_variables = gs;
        kernel_code = p;
      }
    }
