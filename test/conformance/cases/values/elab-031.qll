{ M = module { pub type E = MkE(I64) }; type A = MkA(M.E); match (MkA(M.MkE(4))) { MkA(M.MkE(n)) => n } }
