%{
open Core_tt.Surface
%}

%token <int64> INT
%token <string> ID
%token TRUE FALSE UNIT
%token LET IN FUN IF THEN ELSE
%token ARROW COLON EQUALS
%token LPAREN RPAREN COMMA
%token <string> OP
%token EOF

%start <t> expr_eof

%%

expr_eof:
  | e = expr; EOF { e }

expr:
  | LET; name = ID; ty = option(preceded(COLON, expr)); EQUALS; value = expr; IN; body = expr
    { Let { name; type_ = ty; value; body } }
  | FUN; ps = nonempty_list(param); ARROW; body = expr
    { List.fold_right (fun p acc -> Lam (p, acc)) ps body }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr
    { If { cond; then_; else_ } }
  | e = expr_arrow { e }

expr_arrow:
  | dom = expr_binop; ARROW; cod = expr_arrow { Arrow (dom, cod) }
  | e = expr_binop { e }

expr_binop:
  | lhs = expr_binop; op = OP; rhs = expr_app
    { Ap (Ap (Var op, lhs), rhs) }
  | e = expr_app { e }

expr_app:
  | f = expr_app; a = expr_primary { Ap (f, a) }
  | e = expr_primary { e }

expr_primary:
  | n = INT { Atom (I64 n) }
  | TRUE { Atom (Bool true) }
  | FALSE { Atom (Bool false) }
  | UNIT { Atom Unit }
  | name = ID { Var name }
  | LPAREN; e = expr; COLON; ty = expr; RPAREN { Annotated { inner = e; typ = ty } }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; e1 = expr; COMMA; rest = separated_nonempty_list(COMMA, expr); RPAREN
    { Prod (e1 :: rest) }

param:
  | name = ID { { name; type_ = None } }
  | LPAREN; name = ID; COLON; ty = expr; RPAREN { { name; type_ = Some ty } }
