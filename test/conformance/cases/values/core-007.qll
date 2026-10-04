# lambda shadows outer let
{ x = 1; (fn(x) { x } : I64 -> I64)(7) }
