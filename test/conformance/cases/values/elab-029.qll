{ M = module { pub type A = MkA(EffectRow) and EffectRow = MkE(I64) }; f = fn(a : M.A) { match (a) { M.MkA(M.MkE(n)) => n } }; f(M.MkA(M.MkE(3))) }
