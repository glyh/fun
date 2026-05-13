%{
open Core_tt.Surface
%}

%token <int64> INT
%token <string> ID
%token TRUE FALSE UNIT
%token LET REC IN FUN IF THEN ELSE MATCH WITH
%token STRUCT END OPEN PUB TYPE
%token ARROW COLON EQUALS SEMI
%token LPAREN RPAREN COMMA DOT BAR
%token LBRACE RBRACE
%token <string> OP
%token EOF

%start <t> expr_eof

%%

expr_eof:
  | e = expr; EOF { e }

expr:
  | LET; name = ID; ty = option(preceded(COLON, expr)); EQUALS; value = expr; IN; body = expr
    { Let { name; type_ = ty; value; body; recursive = false } }
  | LET; REC; name = ID; ty = option(preceded(COLON, expr)); EQUALS; value = expr; IN; body = expr
    { Let { name; type_ = ty; value; body; recursive = true } }
  | FUN; ps = nonempty_list(param); ARROW; body = expr
    { List.fold_right (fun p acc -> Lam (p, acc)) ps body }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr
    { If { cond; then_; else_ } }
  | STRUCT; flds = list(struct_field_decl); bnds = separated_list(SEMI, struct_binding); END
    { Struct { con_fields = flds; bindings = bnds } }
  | TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl); IN; body = expr
    { TypeDef { name; params; ctors; body } }
  | OPEN; name = ID; IN; body = expr
    { Open (name, body) }
  | MATCH; scrut = expr_app; WITH; brs = separated_nonempty_list(BAR, branch); END
    { Match (scrut, brs) }
  | e = expr_arrow { e }

branch:
  | p = pat; ARROW; body = expr_arrow { (p, body) }

pat:
  | name = ID; LPAREN; sub = separated_nonempty_list(COMMA, pat); RPAREN
    { PatCon (name, sub) }
  | name = ID
    { if String.equal name "_" then PatWild
      else if Char.uppercase_ascii name.[0] = name.[0] then PatCon (name, [])
      else PatBind name }

struct_field_decl:
  | name = ID; COLON; ty = expr; SEMI { (name, ty) }

struct_binding:
  | PUB; LET; name = ID; EQUALS; value = expr { LetBinding { name; value; public = true } }
  | LET; name = ID; EQUALS; value = expr { LetBinding { name; value; public = false } }
  | PUB; TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl)
    { TypeBinding { name; params; ctors; public = true } }
  | TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl)
    { TypeBinding { name; params; ctors; public = false } }

ctor_decl:
  | name = ID; payload = option(expr) { (name, payload) }

expr_arrow:
  | LBRACE; dom = expr; RBRACE; ARROW; cod = expr_arrow { Arrow (Implicit, dom, cod) }
  | dom = expr_binop; ARROW; cod = expr_arrow { Arrow (Explicit, dom, cod) }
  | e = expr_binop { e }

expr_binop:
  | lhs = expr_binop; op = OP; rhs = expr_app
    { Ap (Ap (Var op, Explicit, lhs), Explicit, rhs) }
  | e = expr_app { e }

expr_app:
  | f = expr_app; LBRACE; a = expr_proj; RBRACE { Ap (f, Implicit, a) }
  | f = expr_app; a = expr_proj { Ap (f, Explicit, a) }
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
