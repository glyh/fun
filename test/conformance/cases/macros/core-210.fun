# type-directed default for I64
{
       macro default[A](_) : Expr(A) {
         match (A) {
         RExpr(I64) => Syntax.i64(0),
         RExpr(Bool) => quote(False),
         _ => { _ = A; Syntax.i64(42) }
         }
       };
       { x : I64 = default(0); x }
     }
