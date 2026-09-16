{ M = module { pub rec A = struct { b : Option(B) } and B = struct { n : I64; a : Option(A) } }; (M.B{ n = 5, a = Some(M.A{ b = None }) }).n }
