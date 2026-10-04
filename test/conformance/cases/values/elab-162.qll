{ M = module { pub rec A = enum { MkA(B), StopA } and B = enum { MkB(A), StopB } };
            match (M.A.MkA(M.B.MkB(M.A.StopA))) { M.A.MkA(M.B.MkB(_)) => 1, _ => 0 } }
