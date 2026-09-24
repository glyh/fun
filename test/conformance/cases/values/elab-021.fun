# a named impl is a member of its module: M.eq_i64 denotes the Eq(I64) impl
{ trait Eq(A) = sig { eq : A -> A -> Bool }; M = module { pub impl eq_bool : Eq(Bool) = module { eq = fn(x, y) { True } }; pub impl eq_i64 : Eq(I64) = module { eq = fn(x, y) { False } } }; same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; if (same[I64, M.eq_i64](1, 1)) { 1 } else { 2 } }
