# macro : Expr(A) works
{
       macro mk[A](_) : Expr(A) { { _ = A; Syntax.i64(1) } };
       { x : I64 = mk(0); x }
     }
