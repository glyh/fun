{ rec fact : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; F = fn(m : I64) { if (m == 4) { I64 } else { Bool } }; g = fn(n : I64, y : F(fact(n))) { (y : F(fact(n))) }; 2 }
