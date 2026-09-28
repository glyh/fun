# type-case refinement reaches the entries an open ADT introduced
({ Opt = fn(A : Type) { enum { Some2(A), None2 } };
   f : [T : Type] -> T -> I64 = fn[T](x) {
     open Opt(T);
     match (T) { I64 => { o : Opt(I64) = Some2(x); 1 }, _ => 0 } };
   f(7) })
