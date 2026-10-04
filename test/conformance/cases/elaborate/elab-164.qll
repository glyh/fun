{ M = module { pub rec A = enum { MkA(B) } and B = struct { a : A } }; 1 }
