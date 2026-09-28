{ trait Eq(a) = sig { eq : a -> a -> Bool }; impl Eq(I64) = module { eq = fn(x, y) { x == y } }; 0 }
