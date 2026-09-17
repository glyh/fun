# the type a typed macro output promises meets the expected type
{ macro m(_) : Expr(I64) { Syntax.i64(1) }; x : Bool = m(0); x }
