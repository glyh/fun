# a typed call in a type annotation
{ macro ty(_) : Expr(Type) { quote(I64) }; (3 : ty(1)) }
