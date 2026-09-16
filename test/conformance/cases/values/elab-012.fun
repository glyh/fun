{ M = module { pub type A = MkA(B) | NoA and B = MkB(A) | NoB }; match (M.MkA(M.MkB(M.NoA))) { M.MkA(M.MkB(M.NoA)) => 1, _ => 0 } }
