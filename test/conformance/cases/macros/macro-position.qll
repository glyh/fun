# an expression macro used where declarations go is refused
{ macro e(_) : Expr(_) { quote { pub x = 1 } }; M = module { e(0) }; M.x }
