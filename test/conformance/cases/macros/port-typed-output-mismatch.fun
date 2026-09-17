# a type-aware macro output is checked at the type it promised
{ macro m(_) : Expr(I64) { quote(True) }; m(0) }
