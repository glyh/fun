# `Type` is a pattern form: the universe is no AtomTy
{ classify = fn(T : Type) { match (T) { Type => 1, Option(Type) => 2, _ => 0 } };
  classify(Type) + (classify(Option(Type)) + classify(Option(I64))) }
