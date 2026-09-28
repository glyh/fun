# an arrow pattern: the domain and codomain are type patterns (`I64 -> a` binds `a`)
{ classify = fn(T : Type) { match (T) { I64 -> Bool => 1, I64 -> a => 2, _ => 0 } };
  classify(I64 -> Bool) + (classify(I64 -> I64) + classify(Bool)) }
