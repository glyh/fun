%{
open Surface

type record_pat_entry =
  | RecordPatField of string * pat option
  | RecordPatWildcard

let split_record_pat_entries entries =
  let rec go acc = function
    | [] -> (List.rev acc, false)
    | [RecordPatWildcard] -> (List.rev acc, true)
    | RecordPatWildcard :: _ -> failwith "record pattern wildcard must be last"
    | RecordPatField (name, pat) :: rest -> go ((name, pat) :: acc) rest
  in
  match go [] entries with
  | [], _ -> failwith "record pattern must have at least one named field"
  | result -> result
%}

%token <int64> INT
%token <char> CHAR
%token <string> ID
%token TRUE FALSE UNIT
%token LET REC IN FUN IF THEN ELSE MATCH WITH
%token STRUCT END OPEN PUB TYPE
%token ARROW COLON EQUALS SEMI
%token LPAREN RPAREN COMMA DOT BAR
%token LBRACE RBRACE
%token <string> CMP_OP ADD_OP MUL_OP
%token EOF

%start <t> expr_eof

%%

expr_eof:
  | e = expr; EOF { e }

expr:
  | LET; name = binding_name; ty = option(preceded(COLON, expr)); EQUALS; value = expr; IN; body = expr
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
  | p = pat; ARROW; body = expr { (p, body) }

binding_name:
  | name = ID { name }
  | LPAREN; op = operator; RPAREN { op }

pat:
  | lhs = pat_atom; BAR; rhs = pat { PatOr (lhs, rhs) }
  | p = pat_atom { p }

pat_atom:
  | typ = ID; LBRACE; entries = separated_nonempty_list(SEMI, record_pat_entry); RBRACE
    { let fields, partial = split_record_pat_entries entries in
      PatRecord { typ; fields; partial } }
  | n = INT { PatAtom (I64 n) }
  | c = CHAR { PatAtom (Char c) }
  | TRUE { PatAtom (Bool true) }
  | FALSE { PatAtom (Bool false) }
  | UNIT { PatAtom Unit }
  | LPAREN; p = pat; RPAREN { p }
  | LPAREN; p = pat; COMMA; rest = separated_nonempty_list(COMMA, pat); RPAREN
    { PatProd (p :: rest) }
  | name = ID; LPAREN; sub = separated_nonempty_list(COMMA, pat); RPAREN
    { PatCon (name, sub) }
  | name = ID
    { if String.equal name "_" then PatWild
      else match name with
      | "I64" -> PatType Core.TI64
      | "Bool" -> PatType TBool
      | "Unit" -> PatType TUnit
      | "Char" -> PatType TChar
      | _ -> if Char.uppercase_ascii name.[0] = name.[0] then PatCon (name, []) else PatBind name }

record_pat_entry:
  | name = ID; EQUALS; p = pat { RecordPatField (name, Some p) }
  | name = ID
    { if String.equal name "_" then RecordPatWildcard else RecordPatField (name, None) }

struct_field_decl:
  | name = ID; COLON; ty = expr; SEMI { (name, ty) }

struct_binding:
  | PUB; LET; name = binding_name; EQUALS; value = expr { LetBinding { name; value; public = true } }
  | LET; name = binding_name; EQUALS; value = expr { LetBinding { name; value; public = false } }
  | PUB; TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl)
    { TypeBinding { name; params; ctors; public = true } }
  | TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl)
    { TypeBinding { name; params; ctors; public = false } }

ctor_decl:
  | name = ID; payload = option(expr) { (name, payload) }

expr_arrow:
  | LBRACE; name = ID; COLON; dom = expr; RBRACE; ARROW; cod = expr_arrow { Arrow (Implicit, Some name, dom, cod) }
  | LBRACE; dom = expr; RBRACE; ARROW; cod = expr_arrow { Arrow (Implicit, None, dom, cod) }
  | dom = expr_cmp; ARROW; cod = expr_arrow { Arrow (Explicit, None, dom, cod) }
  | e = expr_cmp { e }

expr_cmp:
  | lhs = expr_cmp; op = cmp_op; rhs = expr_add
    { Ap (Ap (Var op, Explicit, lhs), Explicit, rhs) }
  | e = expr_add { e }

expr_add:
  | lhs = expr_add; op = add_op; rhs = expr_mul
    { Ap (Ap (Var op, Explicit, lhs), Explicit, rhs) }
  | e = expr_mul { e }

expr_mul:
  | lhs = expr_mul; op = mul_op; rhs = expr_app
    { Ap (Ap (Var op, Explicit, lhs), Explicit, rhs) }
  | e = expr_app { e }

cmp_op:
  | op = CMP_OP { op }

add_op:
  | op = ADD_OP { op }

mul_op:
  | op = MUL_OP { op }

operator:
  | op = CMP_OP { op }
  | op = ADD_OP { op }
  | op = MUL_OP { op }

expr_app:
  | typ = expr_app; LBRACE; fields = separated_nonempty_list(SEMI, record_expr_field); RBRACE
    { RecordConstruct { typ; fields } }
  | f = expr_app; LBRACE; a = expr_proj; RBRACE { Ap (f, Implicit, a) }
  | f = expr_app; a = expr_proj { Ap (f, Explicit, a) }
  | e = expr_proj { e }

record_expr_field:
  | name = ID; EQUALS; value = expr { (name, value) }

expr_proj:
  | e = expr_proj; DOT; i = INT { Proj (e, Int64.to_int i) }
  | e = expr_proj; DOT; name = ID { FieldAccess (e, name) }
  | e = expr_primary { e }

expr_primary:
  | n = INT { Atom (I64 n) }
  | c = CHAR { Atom (Char c) }
  | TRUE { Atom (Bool true) }
  | FALSE { Atom (Bool false) }
  | UNIT { Atom Unit }
  | name = ID { Var name }
  | LPAREN; op = operator; RPAREN { Var op }
  | LPAREN; e = expr; COLON; ty = expr; RPAREN { Annotated { inner = e; typ = ty } }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; e1 = expr; COMMA; rest = separated_nonempty_list(COMMA, expr); RPAREN
    { Prod (e1 :: rest) }

param:
  | name = ID { { name; type_ = None; explicitness = Explicit } }
  | LPAREN; name = ID; COLON; ty = expr; RPAREN { { name; type_ = Some ty; explicitness = Explicit } }
  | LBRACE; name = ID; COLON; ty = expr; RBRACE { { name; type_ = Some ty; explicitness = Implicit } }
