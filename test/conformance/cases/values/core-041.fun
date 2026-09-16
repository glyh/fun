# rec count
{ rec f : I64 -> I64 = fn(n) { if (n == 5) { 5 } else { f(n + 1) } }; f(0) }
