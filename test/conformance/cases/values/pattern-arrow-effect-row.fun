# an effect-annotated explicit arrow still matches `a -> b`, with b the result
{ effect E = sig { op : I64 -> I64 };
  classify = fn(T : Type) { match (T) { a -> b => match (b) { Bool => 10, _ => 0 }, _ => 0 } };
  classify(I64 ->{E} Bool) }
