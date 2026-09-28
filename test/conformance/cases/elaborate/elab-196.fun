{ trait Eq(a) = sig { eq : a -> a -> Bool }; same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(True, False) }
