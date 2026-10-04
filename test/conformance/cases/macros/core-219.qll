# a placement at a convertible type
{ macro inner(y : Expr(I64)) : Expr(I64) { y };
     macro twice(x : Expr(I64)) : Expr(I64) { quote($x + $x) };
     macro under(x : Expr(I64)) : Expr(I64) { quote((fn(y : I64) { $x + y })(1)) };
     macro at_alias(x : Expr(I64)) : Expr(I64) { quote(($x : (fn(t : Type) { t })(I64))) };
     macro rebuild(x : Expr(I64)) : Expr(I64) { match (x) { Syntax.Atom(v) => Syntax.atom_val(v), _ => x } }; z = 41; at_alias(inner(z)) }
