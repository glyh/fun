%{
open Core_tt.Surface
%}

%token <int64> INT
%token <string> ID
%token TRUE FALSE UNIT
%token LET IN FUN IF THEN ELSE
%token STRUCT END OPEN PUB TYPE
%token ARROW COLON EQUALS SEMI
%token LPAREN RPAREN COMMA DOT BAR
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
  | STRUCT; flds = list(struct_field_decl); bnds = separated_list(SEMI, struct_binding); END
    { Struct { con_fields = flds; bindings = bnds } }
  | TYPE; name = ID; EQUALS; ctors = separated_nonempty_list(BAR, ID); IN; body = expr
    { TypeDef (name, ctors, body) }
  | OPEN; name = ID; IN; body = expr
    { Open (name, body) }
  | e = expr_arrow { e }

struct_field_decl:
  | name = ID; COLON; ty = expr; SEMI { (name, ty) }

struct_binding:
  | PUB; LET; name = ID; EQUALS; value = expr { LetBinding { name; value; public = true } }
  | LET; name = ID; EQUALS; value = expr { LetBinding { name; value; public = false } }
  | PUB; TYPE; name = ID; EQUALS; ctors = separated_nonempty_list(BAR, ID)
    { TypeBinding { name; ctors; public = true } }
  | TYPE; name = ID; EQUALS; ctors = separated_nonempty_list(BAR, ID)
    { TypeBinding { name; ctors; public = false } }

expr_arrow:
  | dom = expr_binop; ARROW; cod = expr_arrow { Arrow (dom, cod) }
  | e = expr_binop { e }

expr_binop:
  | lhs = expr_binop; op = OP; rhs = expr_app
    { Ap (Ap (Var op, lhs), rhs) }
  | e = expr_app { e }

expr_app:
  | f = expr_app; a = expr_proj { Ap (f, a) }
  | e = expr_proj { e }

expr_proj:
  | e = expr_proj; DOT; i = INT { Proj (e, Int64.to_int i) }
  | e = expr_proj; DOT; name = ID { FieldAccess (e, name) }
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
