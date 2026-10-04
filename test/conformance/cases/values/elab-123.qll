{ rec fact = fn(n : I64) : I64 { if (n == 0) { 1 } else { n * fact(n - 1) } }; fact(5) }
