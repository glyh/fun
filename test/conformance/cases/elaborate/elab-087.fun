{ trait Eq(A) = sig { eq : A -> A -> Bool }; Ordered = sig { T : Type; eq_T : impl Eq(T) }; same = fn(s : Ordered) { 1 }; same(module { pub T = I64 }) }
