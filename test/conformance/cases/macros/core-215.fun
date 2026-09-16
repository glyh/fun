# a typed call inside a lambda body its output adds
{
       macro inner(x : Expr(I64)) : Expr(I64) { x };
       macro under(x : Expr(I64)) : Expr(I64) { quote((fn(z : I64) { $x })(0)) };
       z = 5;
       under(inner(z))
     }
