type t = Expr | TypeExpr | Pattern | Decl | ModuleItem | Block

let to_string = function
  | Expr -> "expression"
  | TypeExpr -> "type"
  | Pattern -> "pattern"
  | Decl -> "declaration"
  | ModuleItem -> "module item"
  | Block -> "block"
