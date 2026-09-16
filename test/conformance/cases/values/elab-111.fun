{ k = 2; rec down = fn(n : I64) { if (n == 0) { 0 } else { k + back(n - 1) } } and back = fn(n : I64) { if (n == 0) { 0 } else { down(n - 1) } }; down(4) }
