# a type pattern names an enclosing type binder: `^A` and the bare `A` are the same reference
{ f = fn[A : Type](T : Type) { match (T) { Option(^A) => 1, _ => 0 } };
  g = fn[A : Type](T : Type) { match (T) { Option(A) => 2, _ => 0 } };
  f[I64](Option(I64)) + g[Bool](Option(Bool)) }
