{ trait Eq(A) = sig { eq : A -> A -> Bool }; M = module { pub type C = R; impl eq_C : Eq(C) = module { eq = fn(x, y) { True } } }; M.eq_C }
