# { x = 1; macro m[A](e) : Expr(A) { { _ = A; quote((fn(x) { $e })(2)) } }; y : I64 = m(x); y }
{ x = 1; macro m[A](e) : Expr(A) { { _ = A; quote((fn(x) { $e })(2)) } }; y : I64 = m(x); y }
