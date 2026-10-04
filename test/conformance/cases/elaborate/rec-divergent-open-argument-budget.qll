# a fixpoint unfolds on an unknown argument too, under the budget
{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(n : I64, y : loop(n)) { 1 }; 2 }
