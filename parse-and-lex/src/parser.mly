%{ open Ast %}

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF
%left PLUS MINUS
%left TIMES DIV
%start <Ast.ast> parse_ast
%%

parse_ast:
  | e = expr; EOF
    { e }
  ;

expr:
  | i = INT
    { Int i }
  | LPAREN; e = expr; RPAREN
    { e }
  | e0 = expr; PLUS; e1 = expr
    { Binop (Add, e0, e1) }
  | e0 = expr; MINUS; e1 = expr
    { Binop (Sub, e0, e1) }
  | e0 = expr; TIMES; e1 = expr
    { Binop (Mul, e0, e1) }
  | e0 = expr; DIV; e1 = expr
    { Binop (Div, e0, e1) }
  ;
