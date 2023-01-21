%{
open Ast
%}

%token TRUE
%token FALSE
%token <string> ID
%token <string> CONST
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ

%token LPAREN
%token RPAREN
%token EOF

%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO

%left SEQ


%nonassoc ELSE, DO

%left OR
%left AND
%left NOT

%left ADD, SUB
%left MUL
%left EQ, LEQ

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | x = ID { Var(x) }
  | c = CONST { Const(int_of_string c) }
  | NOT; e=expr { Not(e) }
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
  | LPAREN; e=expr; RPAREN { e }
;

cmd:
  | SKIP {Skip}
  | x = ID; ASSIGN; e = expr { Assign(x, e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd { While(e, c) }
  | LPAREN; c = cmd; RPAREN; { c }
;
