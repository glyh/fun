{ M = module { pub trait Same(A) = sig { same : A -> A -> I64 } }; impl M.Same(I64) = module { same = fn(x, y) { 7 } }; f : [A : M.Same] -> A -> I64 = fn[A : Type](x) { M.Same.same(x, x) }; f(1) }
