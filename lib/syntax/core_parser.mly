%{
open Surface
module A = Atom

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

let bare_pat_name path name =
  if String.equal name "_" then PatWild
  else match name with
  | "I64" when path = [] -> PatType Core.TI64
  | "Bool" when path = [] -> PatType TBool
  | "Unit" when path = [] -> PatType TUnit
  | "Char" when path = [] -> PatType TChar
  | _ -> if path <> [] || Char.uppercase_ascii name.[0] = name.[0] then PatCon (path, name, []) else PatBind name
%}

%token <int64> INT
%token <char> CHAR
%token <string> ID
%token <string> STRING
%token TRUE FALSE UNIT
%token LET REC IN FUN IF THEN ELSE MATCH WITH
%token STRUCT END OPEN PUB TYPE EFFECT METHOD IMPORT SELF SELF_TYPE
%token ARROW COLON EQUALS SEMI
%token LPAREN RPAREN COMMA DOT BAR
%token LBRACE RBRACE
%token <string> CMP_OP ADD_OP MUL_OP
%token EOF

%start <t> expr_eof
%start <t> module_eof

%%

expr_eof:
  | e = expr; EOF { e }

module_eof:
  | flds = list(struct_field_decl); bnds = separated_list(SEMI, struct_binding); EOF
    { Struct { con_fields = flds; bindings = bnds } }

expr:
  | LET; name = binding_name; ty = option(preceded(COLON, expr)); EQUALS; rhs = let_rhs; IN; body = expr
    { match rhs with
      | `Value value -> Let { name; type_ = ty; value; body; recursive = false }
      | `Effect ops -> EffectDef { name; params = []; ops; body } }
  | LET; name = ID; params = nonempty_list(ID); EQUALS; EFFECT;
    ops = separated_nonempty_list(SEMI, effect_op_decl); END; IN; body = expr
    { EffectDef { name; params; ops; body } }
  | LET; REC; name = ID; ty = option(preceded(COLON, expr)); EQUALS; value = expr; IN; body = expr
    { Let { name; type_ = ty; value; body; recursive = true } }
  | FUN; ps = nonempty_list(param); ARROW; body = expr
    { List.fold_right (fun p acc -> Lam (p, acc)) ps body }
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr
    { If { cond; then_; else_ } }
  | STRUCT; flds = list(struct_field_decl); bnds = separated_list(SEMI, struct_binding); END
    { Struct { con_fields = flds; bindings = bnds } }
  | TYPE; name = ID; params = list(ID); EQUALS;
    LBRACE; fields = separated_nonempty_list(SEMI, record_type_field_decl); RBRACE;
    IN; body = expr
    { RecordTypeDef { name; params; fields; body } }
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
  | typ = dotted_id; LBRACE; entries = separated_nonempty_list(SEMI, record_pat_entry); RBRACE
    { let path, typ = typ in
      let fields, partial = split_record_pat_entries entries in
      PatRecord { typ_path = path; typ; fields; partial } }
  | n = INT { PatAtom (A.I64 n) }
  | c = CHAR { PatAtom (A.Char c) }
  | TRUE { PatAtom (A.Bool true) }
  | FALSE { PatAtom (A.Bool false) }
  | UNIT { PatAtom A.Unit }
  | LPAREN; p = pat; RPAREN { p }
  | LPAREN; p = pat; COMMA; rest = separated_nonempty_list(COMMA, pat); RPAREN
    { PatProd (p :: rest) }
  | name = dotted_id; LPAREN; sub = separated_nonempty_list(COMMA, pat); RPAREN
    { let path, name = name in PatCon (path, name, sub) }
  | name = dotted_id
    { let path, name = name in bare_pat_name path name }

dotted_id:
  | name = ID { ([], name) }
  | prefix = dotted_id; DOT; name = ID { let path, last = prefix in (path @ [last], name) }

record_pat_entry:
  | name = ID; EQUALS; p = pat { RecordPatField (name, Some p) }
  | name = ID
    { if String.equal name "_" then RecordPatWildcard else RecordPatField (name, None) }

struct_field_decl:
  | name = ID; COLON; ty = expr; SEMI { (name, ty) }

record_type_field_decl:
  | name = ID; COLON; ty = expr { (name, ty) }

effect_op_decl:
  | name = ID; COLON; input = type_expr; ARROW; output = type_expr
    { { name; input; output } }

let_rhs:
  | EFFECT; ops = separated_nonempty_list(SEMI, effect_op_decl); END { `Effect ops }
  | value = expr { `Value value }

struct_binding:
  | PUB; LET; name = binding_name; EQUALS; rhs = let_rhs
    { match rhs with
      | `Value value -> LetBinding { name; value; public = true }
      | `Effect ops -> EffectBinding { name; params = []; ops; public = true } }
  | PUB; LET; name = ID; params = nonempty_list(ID); EQUALS; EFFECT;
    ops = separated_nonempty_list(SEMI, effect_op_decl); END
    { EffectBinding { name; params; ops; public = true } }
  | LET; name = binding_name; EQUALS; rhs = let_rhs
    { match rhs with
      | `Value value -> LetBinding { name; value; public = false }
      | `Effect ops -> EffectBinding { name; params = []; ops; public = false } }
  | LET; name = ID; params = nonempty_list(ID); EQUALS; EFFECT;
    ops = separated_nonempty_list(SEMI, effect_op_decl); END
    { EffectBinding { name; params; ops; public = false } }
  | PUB; METHOD; name = ID; params = list(param); ARROW; body = expr
    { MethodBinding { name; params; body; public = true } }
  | METHOD; name = ID; params = list(param); ARROW; body = expr
    { MethodBinding { name; params; body; public = false } }
  | PUB; TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl)
    { TypeBinding { name; params; ctors; public = true } }
  | TYPE; name = ID; params = list(ID); EQUALS;
    ctors = separated_nonempty_list(BAR, ctor_decl)
    { TypeBinding { name; params; ctors; public = false } }

ctor_decl:
  | name = ID; payload = option(type_expr) { (name, payload) }

type_expr:
  | lhs = type_expr; op = MUL_OP; rhs = type_app
    { if not (String.equal op "*") then failwith "only * is valid in type product syntax";
      match lhs with
      | ProdTy elems -> ProdTy (elems @ [ rhs ])
      | _ -> ProdTy [ lhs; rhs ] }
  | e = type_app { e }

type_app:
  | f = type_app; a = type_atom { Ap (f, Explicit, a) }
  | e = type_atom { e }

type_atom:
  | name = ID { Var name }
  | LPAREN; e = type_expr; RPAREN { e }

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
  | n = INT { Atom (A.I64 n) }
  | c = CHAR { Atom (A.Char c) }
  | TRUE { Atom (A.Bool true) }
  | FALSE { Atom (A.Bool false) }
  | UNIT { Atom A.Unit }
  | SELF { Self }
  | SELF_TYPE { SelfType }
  | IMPORT; path = STRING { Import path }
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
