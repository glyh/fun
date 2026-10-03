# the crash-class spelling of a negative tuple arity: g(Tuple(0 - 5)) forces the tuple_arity chain through unification, and must be refused, not crash
{ g = fn(x) { x }; g(Tuple(0 - 5)); 1 }
