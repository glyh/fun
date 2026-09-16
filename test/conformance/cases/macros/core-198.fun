# type-aware binder solved from the expected type
{
       macro default[A](_) : Expr(A) {
         { _ = A; Syntax.i64(1) }
       };
       { x : I64 = default(0); x }
     }
