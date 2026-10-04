# a tuple type pattern in both spellings: `(a, b)` and `Tuple(2, a, b)`, arity-keyed
{ classify = fn(T : Type) { match (T) { Tuple(3, a, b, c) => 2, (a, b) => 1, _ => 0 } };
  classify(Tuple(2, I64, Bool)) + (classify(Tuple(3, I64, Bool, Char)) + classify(I64)) }
