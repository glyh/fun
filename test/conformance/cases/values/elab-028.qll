{ type E = Other(I64); M = module { pub type A = MkA(E) and E = MkE(I64) }; f = fn(a : M.A) { match (a) { M.MkA(M.MkE(n)) => n } }; f(M.MkA(M.MkE(3))) }
