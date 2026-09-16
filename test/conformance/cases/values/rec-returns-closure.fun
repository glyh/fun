# a recursive function passed around and returned
{ rec k = fn(x : I64) { fn(y : I64) { x } }; f = fn(g : I64 -> I64 -> I64) { g(7)(8) }; f(k) }
