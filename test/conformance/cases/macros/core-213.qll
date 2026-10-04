# a typed argument checks
{ macro twice(x : Expr(I64)) : Expr(I64) { quote($x + $x) }; twice(21) }
