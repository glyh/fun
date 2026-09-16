# type-aware output expands nested macro
{
       macro one(_) { Syntax.i64(1) };
       macro m[A](e) : Expr(A) { { _ = A; quote(one($e)) } };
       y : I64 = m(0);
       y
     }
