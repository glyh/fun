{ rec k : I64 -> Type = fn(n) { if (n == 0) { I64 } else { k(n - 1) } }; g = fn(y : k(3)) { y + 1 }; 2 }
