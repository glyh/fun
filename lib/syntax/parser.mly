%{
  [@@@coverage exclude_file]
  open Ast

%}

%token <string> ID
%token <int64> I64
%token LET REC IN IF THEN ELSE FUN 
%token ARROW LPAREN RPAREN ASSIGN COLON DOUBLESEMI UNIT
%token TRUE FALSE
%token EOF

%right ARROW

%start <Binding.t list> toplevel_eof
%start <Expr.t> expr_eof

%%

toplevel_eof:
  | bindings=separated_list(DOUBLESEMI, binding) EOF { bindings }

type_:
  | LPAREN type_ RPAREN { $2 }
  | ID { Type.T.Con $1 }
  | type_ ARROW type_ { Type.T.Arrow ($1, $3) }

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

binding: 
  | LET rec_=option(REC) name=ID params=list(param) ASSIGN rhs=expr {
    let value =
      List.fold_right (fun param acc -> Expr.Lam (param, acc)) params rhs
    in
    Binding.{ recursive = Option.is_some rec_; name; type_ = None; value }
  }

atom: 
  | ID { Expr.Var $1 }
  | I64 { Expr.Atom (I64 $1) }
  | UNIT { Expr.Atom Unit }
  | TRUE { Expr.Atom (Bool true) }
  | FALSE { Expr.Atom (Bool false) }
  | LPAREN inner=expr typ=option(type_annotation) RPAREN { 
    match typ with
    | None -> inner
    | Some typ -> Expr.(Annotated { inner; typ })
  }

app:
  | atom { $1 }
  | app atom { Expr.Ap ($1, $2) }

expr:
  | app { $1 }
  | binding=binding IN body=expr {
    Expr.Let { binding; body }
  }
  | IF cond=expr THEN then_branch=expr ELSE else_branch=expr {
      Expr.If { cond; then_ = then_branch; else_ = else_branch }
    }
  | FUN params=list(param) ARROW body=expr {
      List.fold_right (fun param acc -> Expr.Lam (param, acc)) params body
    }

expr_eof:
  | expr EOF { $1 }
