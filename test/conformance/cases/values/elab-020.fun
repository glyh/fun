{ M = module { pub type A = MkA(EffectRow) and EffectRow = MkE(I64) }; f = fn(a : M.A) { 1 }; f(M.MkA(M.MkE(3))) }
