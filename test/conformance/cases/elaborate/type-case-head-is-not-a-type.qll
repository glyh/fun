# a bare type-case head that names no type is refused, naming the offending term
{ C = 5; classify = fn(T : Type) { match (T) { C => 1, _ => 0 } }; classify(I64) }
