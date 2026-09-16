{ trait Eq(A) = sig { eq : A -> A -> Bool }; impl Eq(I64) = module { eq = fn(x, y) { x == y } }; 0 }
