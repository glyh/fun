# a divergent fixpoint in a written type is an evaluation budget error, not a hang
{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(y : loop(0)) { 1 }; 2 }
