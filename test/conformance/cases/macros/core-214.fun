# two binders, solved from the arguments
{
       macro first[A, B](a : Expr(A), b : Expr(B)) : Expr(A) {
         match (B) { RExpr(Bool) => a, _ => b }
       };
       first(40, True) + 2
     }
