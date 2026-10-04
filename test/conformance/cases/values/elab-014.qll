{ type A = MkA(B) | NoA and B = MkB(A) | NoB; match (MkA(MkB(NoA))) { MkA(MkB(NoA)) => 1, _ => 0 } }
