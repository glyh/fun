# macro : Expr(_) explicit annotation
{
       macro check(_) : Expr(_) { Syntax.i64(1) };
       check(0)
     }
