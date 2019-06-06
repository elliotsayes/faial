%token <int> UINT
%token <string> ID
%token SEMICOLON PLUS MINUS MULT DIV MOD LT GT GTE LTE OR AND EQ NEQ
%token NOT
%token EOF
%token SYNC RW RO IF
%token LOCS CONST ASSERT COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token LOOP

%left OR
%left AND
(*
%left EQ NEQ
%left LT GT GTE LTE
*)
%left PLUS MINUS
%left MULT DIV MOD

%{ open Proto %}

%start <Proto.kernel> main
%%
main : p = kernel EOF { p };

nexp:
  | i = UINT { Num i }
  | x = ID { Var x }
  | LPAREN n = nexp RPAREN { n }
  | n1 = nexp ; o = nbin ; n2 = nexp { o n1 n2 }

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
  | LOOP x = ID LT n = nexp LBRACE p = proto RBRACE
    { Loop ({range_var=x; range_upper_bound=n}, p) }
  | p = proto SEMICOLON { p }

ids:
  | { [] }
  | x = ID { [x] }
  | x = ID COMMA xs = ids { x :: xs }

locs:
  | LOCS l1 = ids SEMICOLON { l1 }

const:
  | CONST l2 = ids SEMICOLON { l2 }

kernel:
  | l1 = locs
    l2 = loption(const)
    p = proto {
      {
        kernel_locations = l1;
        kernel_variables = l2;
        kernel_code = p;
      }
    }
  | l2 = loption(const)
    l1 = locs
    p = proto {
      {
        kernel_locations = l1;
        kernel_variables = l2;
        kernel_code = p;
      }
    }
