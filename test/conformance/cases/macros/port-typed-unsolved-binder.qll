# a type-aware macro whose type binder nothing solves
{ macro d[A](_) : Expr(A) { { _ = A; Syntax.i64(1) } }; d(0) }
