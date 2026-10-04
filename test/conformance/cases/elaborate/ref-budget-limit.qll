# a finite fixpoint in a written type that exceeds the evaluation budget is refused
{ rec loop : I64 -> Type = fn(n) { if (n == 0) { I64 } else { loop(n - 1) } }; g = fn(y : loop(200000)) { 1 }; 2 }
