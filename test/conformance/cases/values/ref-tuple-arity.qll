# a Tuple(n, ...) type pattern giving fewer component types than n
{ classify = fn(T : Type) { match (T) { Tuple(2, a) => 1, _ => 0 } }; classify(Tuple(2, I64, Bool)) }
