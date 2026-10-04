# calls of two different fixpoints unfold, and diverge under the budget
{ rec l1 : I64 -> I64 = fn(n) { l1(n) }; rec l2 : I64 -> I64 = fn(n) { l2(n) }; g = fn(F : I64 -> Type, n : I64, y : F(l1(n))) { (y : F(l2(n))) }; 2 }
