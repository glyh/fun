# type-case refinement reaches an entry an open introduced
({ M = fn[T : Type](v : T) { module { pub val = v } };
   f : [T : Type] -> T -> I64 = fn[T](x) {
     m = M[T](x);
     open m;
     match (T) { I64 => val, _ => 0 } };
   f(7) })
