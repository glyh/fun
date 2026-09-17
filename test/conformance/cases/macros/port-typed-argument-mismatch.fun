# a typed argument is checked at the type the signature promises
{ macro m(x : Expr(I64)) : Expr(I64) { x }; m(True) }
