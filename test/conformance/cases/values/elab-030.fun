{ M = module { pub type E = MkE(I64); pub type A = MkA(E) }; match (M.MkA(M.MkE(3))) { M.MkA(M.MkE(n)) => n } }
