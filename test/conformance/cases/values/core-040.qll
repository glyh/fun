# fibonacci
{ rec fib : I64 -> I64 = fn(n) { if (n <= 1) { n } else { fib(n - 1) + fib(n - 2) } }; fib(6) }
