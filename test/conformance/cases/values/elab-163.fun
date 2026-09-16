{ rec A = enum { MkA(B), StopA } and B = enum { MkB(A), StopB };
            open A; open B;
            match (MkA(MkB(StopA))) { MkA(MkB(StopA)) => 1, _ => 0 } }
