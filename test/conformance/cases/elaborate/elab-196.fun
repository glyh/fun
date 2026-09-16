{ trait Eq(A) = sig { eq : A -> A -> Bool }; same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(True, False) }
