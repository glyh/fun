# a type-aware macro's Id parameter
{ macro pick[A](n : Id) : Expr(A) { { _ = A; Syntax.RawVar(None, n) } }; x = 3; pick(x) + 1 }
