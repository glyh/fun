# a pattern that is followed by another term is refused, naming the token
{ classify = fn(T : Type) { match (T) { I64 Bool => 1, _ => 0 } }; classify(I64) }
