%{
  [@@@coverage exclude_file]
  open Ast

%}

%token <string> ID
%token <int> INT
%token LET REC IN IF THEN ELSE FUN 
%token ARROW LPAREN RPAREN ASSIGN COLON DOUBLESEMI UNIT
%token EOF

%start <Binding.t list> toplevel_eof

%%

toplevel_eof:
  | bindings=separated_list(DOUBLESEMI, binding) EOF { bindings }

param:
  | name=ID {
    Param.{ name; type_ = None }
  }
  | LPAREN name=ID COLON typ=ID RPAREN {
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
  | INT { Expr.Num $1 }
  | UNIT { Expr.Unit }
  | LPAREN expr RPAREN { $2 }

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
