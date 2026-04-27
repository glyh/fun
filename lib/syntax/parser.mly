%{
  [@@@coverage exclude_file]
  open Ast

  type record_pat_entry =
    | Pat_field of string * Pattern.t option
    | Pat_wildcard

%}

%token <Type.Id.t> ID
%token <Type.Id.t> TY_VAR
%token <int64> I64
%token LET REC IN IF THEN ELSE FUN TYPE MATCH END STRUCT PUB IMPORT SELF OPEN
%token <string> STRING
%token ARROW LPAREN RPAREN ASSIGN COLON DOUBLESEMI UNIT PIPE
%token LBRACKET RBRACKET COMMA
%token LBRACE RBRACE SEMI DOT
%token TRUE FALSE
%token EOF
%token EQ GE LE GT LT
%token ADD SUB MUL
%token UNDERSCORE


%right ARROW

%left PIPE
%left EQ GE LE GT LT
%left ADD SUB
%left MUL

%start <Binding.t list> toplevel_eof
%start <Expr.t> expr_eof
%start <string list * Ast.struct_body * Struct_def.t list> module_eof

%%

toplevel_eof:
  | bindings=separated_list(DOUBLESEMI, binding) EOF { bindings }

type_:
  | ID { Type.Generic.Con (Type.TypeId.make $1, []) }
  | name=ID LBRACKET args=separated_nonempty_list(COMMA, type_) RBRACKET { Type.Generic.Con (Type.TypeId.make name, args) }
  | type_ ARROW type_ { Type.Generic.Arrow ($1, $3) }
  | LPAREN maybe_tuple=separated_nonempty_list(COMMA, type_) RPAREN { 
    match maybe_tuple with
    | [ element0 ] -> 
        element0
    | element0 :: rest -> 
        Type.Generic.Prod (Std.Nonempty_list.init element0 rest)
    | [] -> failwith "Unreachable: nonempty list returns empty in type_!" 
  }
  | TY_VAR { Type.Generic.Var $1 }
  | SELF { Type.Generic.self }

type_annotation:
  | COLON typ=type_ {
    typ
  }  

param:
  | name=ID {
    Param.{ name; type_ = None }
  }
  | LPAREN name=ID typ=type_annotation RPAREN {
    Param.{ name; type_ = Some typ }
  }

type_with_opt_args:
  | name=ID {
    (name, [])
  }
  | name=ID LBRACKET args=separated_nonempty_list(COMMA, TY_VAR) RBRACKET {
    (name, args)
  }

type_rhs:
  | rhs_tag=ID rhs=option(type_) {
    Ast.Adt (Std.Nonempty_list.init (rhs_tag, rhs)  [])
  }
  | rhs_tag=ID rhs=option(type_) PIPE rest=adt_rhs {
    Ast.Adt (Std.Nonempty_list.cons (rhs_tag, rhs) rest)
  }
  | LBRACE fields=separated_nonempty_list(SEMI, record_field_decl) RBRACE {
    match fields with
    | [] -> failwith "Unreachable: nonempty list returns empty in type_rhs!"
    | hd :: rest -> Ast.Record (Std.Nonempty_list.init hd rest)
  }

adt_rhs:
  | rhs_tag=ID rhs=option(type_) {
    Std.Nonempty_list.init (rhs_tag, rhs)  []
  }
  | rhs_tag=ID rhs=option(type_) PIPE rest=adt_rhs {
    Std.Nonempty_list.cons (rhs_tag, rhs) rest
  }

record_field_decl:
  | name=ID COLON typ=type_ { (name, typ) }

binding: 
  | LET rec_=option(REC) name=ID params=list(param) bind_annotation=option(type_annotation) ASSIGN rhs=expr {
    let lam_inner =
      List.fold_right (fun param acc -> Expr.Lam (param, acc)) params rhs
    in
    let lam_wrapped = 
      if Option.is_some rec_ then
        Expr.Fix(Lam(Param.{name; type_ = None }, lam_inner))
      else 
        lam_inner
    in
    Binding.Value { name; type_ = bind_annotation; value = lam_wrapped }
  }
  | TYPE type_and_args=type_with_opt_args ASSIGN rhs=type_rhs {
    let (name, args) = type_and_args in
    Binding.TypeDecl { name; args; rhs }
  }

atom: 
  | I64 { Atom.I64 $1 }
  | UNIT { Atom.Unit }
  | TRUE { Atom.Bool true }
  | FALSE { Atom.Bool false }

pattern:
  | id=ID { Pattern.Bind id }
  | atom { Pattern.Just $1 }
  | LPAREN maybe_tuple=separated_nonempty_list(COMMA, pattern) RPAREN {
    match maybe_tuple with
    | [ element0 ] ->
        element0
    | element0 :: rest ->
        Pattern.Prod (Std.Nonempty_list.init element0 rest)
    | [] -> failwith "Unreachable: nonempty list returns empty in pattern!"
  }
  | id=ID LPAREN RPAREN { Pattern.Tagged ([], id, None) }
  | id=ID UNIT { Pattern.Tagged ([], id, None) }
  | id=ID LPAREN p=pattern RPAREN { Pattern.Tagged ([], id, Some p) }
  | qid=dotted_ids {
    let path, tag = qid in
    Pattern.Tagged (path, tag, None) }
  | qid=dotted_ids LPAREN p=pattern RPAREN {
    let path, tag = qid in
    Pattern.Tagged (path, tag, Some p) }
  | pattern PIPE pattern { Pattern.Union ($1, $3) }
  | UNDERSCORE { Pattern.Any }
  | name=ID LBRACE entries=separated_nonempty_list(SEMI, record_pat_entry) RBRACE {
    let rec split_entries = function
      | [] -> ([], false)
      | [Pat_wildcard] -> ([], true)
      | Pat_wildcard :: _ -> failwith "Wildcard '_' must be the last entry in a record pattern"
      | Pat_field (name, pat) :: rest ->
          let (fields, partial) = split_entries rest in
          ((name, pat) :: fields, partial)
    in
    let (fields, partial) = split_entries entries in
    match fields with
    | [] -> failwith "Record pattern must have at least one named field"
    | hd :: rest -> Pattern.RecordConstruct { path = []; name; fields = Std.Nonempty_list.init hd rest; partial }
  }
  | qid=dotted_ids LBRACE entries=separated_nonempty_list(SEMI, record_pat_entry) RBRACE {
    let rec split_entries = function
      | [] -> ([], false)
      | [Pat_wildcard] -> ([], true)
      | Pat_wildcard :: _ -> failwith "Wildcard '_' must be the last entry in a record pattern"
      | Pat_field (name, pat) :: rest ->
          let (fields, partial) = split_entries rest in
          ((name, pat) :: fields, partial)
    in
    let (path, name) = qid in
    let (fields, partial) = split_entries entries in
    match fields with
    | [] -> failwith "Record pattern must have at least one named field"
    | hd :: rest -> Pattern.RecordConstruct { path; name; fields = Std.Nonempty_list.init hd rest; partial }
  }

dotted_ids:
  | a=ID DOT b=ID { ([a], b) }
  | rest=dotted_ids DOT b=ID { let (path, prev) = rest in (path @ [prev], b) }

record_pat_entry:
  | name=ID { Pat_field (name, None) }
  | name=ID ASSIGN p=pattern { Pat_field (name, Some p) }
  | UNDERSCORE { Pat_wildcard }

expr: 
  | expr_stmt { $1 }

expr_stmt:
  | binding=binding IN body=expr {
    Expr.Let { binding; body }
  }
  | OPEN name=ID IN body=expr {
    Expr.Let { binding = Open name; body }
  }
  | IF cond=expr THEN then_branch=expr ELSE else_branch=expr {
      Expr.If { cond; then_ = then_branch; else_ = else_branch }
    }
  | FUN params=nonempty_list(param) ARROW body=expr {
      List.fold_right (fun param acc -> Expr.Lam (param, acc)) params body
    }
  | MATCH matched=expr_expr branches=nonempty_list(match_branch) END {
    let branches = 
      match branches with
      | [] -> failwith "Unreachable: nonempty list returns empty in expr_stmt > match!" 
      | hd :: rest -> Std.Nonempty_list.init hd rest
    in Expr.Match { matched; branches }
  }
  | expr_expr { $1 }

match_branch:
  | PIPE pat=pattern ARROW body=expr {
    (pat, body)
  }

%inline bin_op:
  | ADD { "+" }
  | SUB { "-" }
  | MUL { "*" }
  | EQ { "==" }
  | GE { ">=" }
  | LE { "<=" }
  | GT { ">" }
  | LT { "<" }
  
expr_expr: 
  | lhs=expr_expr op=bin_op rhs=expr_expr {
    Expr.Ap(Ap(Var op, lhs), rhs)
  }
  | e=expr_app { e }

expr_app:
  | f=expr_postfix xs=list(expr_postfix) {
    List.fold_left
    (fun partial arg -> Expr.Ap(partial, arg))
    f
    xs
  }

expr_postfix:
  | expr_primary { $1 }
  | expr_postfix DOT field=ID { Expr.FieldAccess ($1, field) }
  | expr_postfix DOT name=ID LBRACE fields=separated_nonempty_list(SEMI, record_expr_field) RBRACE {
    let rec collect_path acc expr =
      match expr with
      | Expr.FieldAccess (inner, seg) -> collect_path (seg :: acc) inner
      | Expr.Var root -> root :: acc
      | _ -> failwith "Qualified record construction requires a module path"
    in
    let path = collect_path [] $1 in
    match fields with
    | [] -> failwith "Unreachable: nonempty list returns empty in expr_postfix!"
    | hd :: rest -> Expr.RecordConstruct { path; name; fields = Std.Nonempty_list.init hd rest }
  }

expr_with_opt_annotation:
  | inner=expr typ=option(type_annotation) { 
    match typ with
    | None -> inner
    | Some typ -> Expr.Annotated { inner ; typ }
  }

expr_primary:
  | atom { Expr.Atom $1 }
  | ID { Expr.Var $1 }
  | LPAREN maybe_tuple=separated_nonempty_list(COMMA, expr_with_opt_annotation) RPAREN {
    match maybe_tuple with
    | [ element0 ] ->
        element0
    | element0 :: rest ->
        Expr.Prod (Std.Nonempty_list.init element0 rest)
    | [] -> failwith "Unreachable: nonempty list returns empty in expr_primary!"
  }
  | name=ID LBRACE fields=separated_nonempty_list(SEMI, record_expr_field) RBRACE {
    match fields with
    | [] -> failwith "Unreachable: nonempty list returns empty in expr_primary!"
    | hd :: rest -> Expr.RecordConstruct { path = []; name; fields = Std.Nonempty_list.init hd rest }
  }
  | STRUCT args=option(delimited(LBRACKET, separated_nonempty_list(COMMA, TY_VAR), RBRACKET)) body=struct_body_with_defs END {
    let (body, defs) = body in
    Expr.StructDef { args = (match args with None -> [] | Some a -> a); body; members = defs }
  }
  | IMPORT path=STRING { Expr.Import path }

record_expr_field:
  | name=ID ASSIGN value=expr { (name, value) }

struct_field_decl:
  | name=ID COLON typ=type_ SEMI { (name, typ) }

struct_variant:
  | PIPE tag=ID payload=option(type_) { (tag, payload) }

%inline nonempty_to_nel(X):
  | xs=nonempty_list(X) { match xs with hd :: rest -> Std.Nonempty_list.init hd rest | [] -> failwith "unreachable" }

struct_body_with_defs:
  | defs=list(struct_def) { (Ast.Namespace, defs) }
  | fields=nonempty_to_nel(struct_field_decl) defs=list(struct_def) { (Ast.Fields fields, defs) }
  | variants=nonempty_to_nel(struct_variant) { (Ast.Variants variants, []) }
  | variants=nonempty_to_nel(struct_variant) SEMI defs=nonempty_list(struct_def) { (Ast.Variants variants, defs) }

struct_def:
  | PUB b=binding { Ast.Struct_def.{ vis = Public; binding = b } }
  | b=binding { Ast.Struct_def.{ vis = Private; binding = b } }
  | OPEN name=ID { Ast.Struct_def.{ vis = Private; binding = Open name } }

module_eof:
  | body=struct_body_with_defs EOF { let (body, defs) = body in ([], body, defs) }

expr_eof:
  | expr EOF { $1 }
