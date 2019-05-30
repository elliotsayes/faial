%token <int> UINT
%token <string> ID
%token SEMICOLON PLUS MINUS MULT DIV MOD LT GT GTE LTE OR AND EQ NEQ
%token NOT
%token EOF
%token SYNC RW RO FOR IF
%token LOCS CONST PRE COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK

%left MOD
%left PLUS MINUS        /* lowest precedence */
%left MULT DIV        /* medium precedence */

%{ open Proto %}

%start <Proto.kernel> main
%%
main : p = kernel EOF { p };

nexp:
  | i = UINT { Num i }
  | x = ID { Var x }
  | LPAREN n = nexp RPAREN { n }
  | n1 = nexp PLUS n2 = nexp { Bin (Plus, n1, n2) }
  | n1 = nexp MINUS n2 = nexp { Bin (Minus, n1, n2) }
  | n1 = nexp MULT n2 = nexp { Bin (Mult, n1, n2) }
  | n1 = nexp DIV n2 = nexp { Bin (Div, n1, n2) }
  | n1 = nexp MOD n2 = nexp { Bin (Mod, n1, n2) }
  ;

bexp:
  | LPAREN b = bexp RPAREN { b }
  | n1 = nexp EQ n2 = nexp { n_eq n1 n2 }
  | n1 = nexp NEQ n2 = nexp { n_neq n1 n2 }
  | n1 = nexp LT n2 = nexp { n_lt n1 n2 }
  | n1 = nexp GT n2 = nexp { n_gt n1 n2 }
  | n1 = nexp LTE n2 = nexp { n_le n1 n2 }
  | n1 = nexp GTE n2 = nexp { n_ge n1 n2 }
  | b1 = bexp OR b2 = bexp { b_or b1 b2 }
  | b1 = bexp AND b2 = bexp { b_and b1 b2 }
  | NOT b = bexp { b_not b }
  ;

mode: RW { W } | RO { R };

proto:
  | SYNC { Sync }
  | p1 = proto SEMICOLON p2 = proto
    { Seq (p1, p2) }
  | m = mode x = ID LBRACK n = nexp RBRACK
    { Acc (x, {access_index=n; access_cond=Bool true; access_mode=m}) }
  | m = mode x = ID LBRACK n = nexp RBRACK IF b = bexp
    { Acc (x, {access_index=n; access_cond=b; access_mode=m}) }
  | FOR x = ID LT n = nexp LBRACE p = proto RBRACE
    { Loop ({range_var=x; range_upper_bound=n}, p) }
  | p = proto SEMICOLON { p }
  ;

ids:
  | { [] }
  | x = ID { [x] }
  | x = ID COMMA xs = ids { x :: xs }

locs:
  | LOCS l1 = ids SEMICOLON { l1 }
  | { [] }

const:
  | CONST l2 = ids SEMICOLON { l2 }
  | { [] }

pre:
  | PRE b = bexp SEMICOLON { b }
  | { Bool true }

kernel:
  | l1 = locs
    l2 = const
    b = pre
    p = proto {
      {
        kernel_locations = l1;
        kernel_variables = l2;
        kernel_pre = b;
        kernel_code = p;
      }
    }
