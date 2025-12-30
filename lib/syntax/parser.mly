%{
  [@@@coverage exclude_file]
  open Ast

%}

%token <Type.Id.t> ID
%token <Type.Id.t> TY_VAR
%token <int64> I64
%token LET REC IN IF THEN ELSE FUN TYPE
%token ARROW LPAREN RPAREN ASSIGN COLON DOUBLESEMI UNIT PIPE
%token LBRACKET RBRACKET COMMA
%token TRUE FALSE
%token EOF
%token EQ GE LE GT LT
%token ADD SUB MUL

%right ARROW

%left EQ GE LE GT LT
%left ADD SUB
%left MUL

%start <Binding.t list> toplevel_eof
%start <Expr.t> expr_eof

%%

toplevel_eof:
  | bindings=separated_list(DOUBLESEMI, binding) EOF { bindings }

type_:
  | LPAREN type_ RPAREN { $2 }
  | ID { Type.Generic.Con ($1, []) }
  | name=ID LBRACKET args=separated_nonempty_list(COMMA, type_) RBRACKET { Type.Generic.Con (name, args) }
  | type_ ARROW type_ { Type.Generic.Arrow ($1, $3) }
  | TY_VAR { Type.Generic.Var $1 }

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
    Std.Nonempty_list.init (rhs_tag, rhs)  []
  }
  | rhs_tag=ID rhs=option(type_) PIPE rest=type_rhs {
    Std.Nonempty_list.cons (rhs_tag, rhs) rest
  }

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
    let lam_wrapped_annotated = 
      match bind_annotation with
      | None -> lam_wrapped
      | Some typ -> Expr.Annotated { inner = lam_wrapped; typ }
    in
    Binding.Value { name; type_ = None; value = lam_wrapped_annotated }
  }
  | TYPE type_and_args=type_with_opt_args ASSIGN rhs=type_rhs {
    let (name, args) = type_and_args in
    Binding.TypeDecl { name; args; rhs }
  }

expr: 
  | expr_stmt { $1 }
    
expr_stmt:
  | binding=binding IN body=expr {
    Expr.Let { binding; body }
  }
  | IF cond=expr THEN then_branch=expr ELSE else_branch=expr {
      Expr.If { cond; then_ = then_branch; else_ = else_branch }
    }
  | FUN params=nonempty_list(param) ARROW body=expr {
      List.fold_right (fun param acc -> Expr.Lam (param, acc)) params body
    }
  | expr_expr { $1 }

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
  | f=expr_primary xs=list(expr_primary) {
    List.fold_left 
    (fun partial arg -> Expr.Ap(partial, arg))
    f
    xs
  }

expr_primary: 
  | ID { Expr.Var $1 }
  | I64 { Expr.Atom (I64 $1) }
  | UNIT { Expr.Atom Unit }
  | TRUE { Expr.Atom (Bool true) }
  | FALSE { Expr.Atom (Bool false) }
  | LPAREN inner=expr typ=option(type_annotation) RPAREN { 
    match typ with
    | None -> inner
    | Some typ -> Expr.Annotated { inner; typ }
  }

expr_eof:
  | expr EOF { $1 }
