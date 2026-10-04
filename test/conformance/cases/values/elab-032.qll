{ type E = MkE(I64); M = module { pub type A = MkA(E) }; match (M.MkA(MkE(5))) { M.MkA(MkE(n)) => n } }
