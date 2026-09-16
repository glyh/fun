{ M = module { pub type A = MkA(E) and E = MkE(I64) }; match (M.MkA(M.MkE(3))) { M.MkA(M.MkE(n)) => n } }
