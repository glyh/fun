# rec sum
{ rec sum : I64 -> I64 = fn(n) { if (n == 0) { 0 } else { sum(n - 1) + n } }; sum(5) }
