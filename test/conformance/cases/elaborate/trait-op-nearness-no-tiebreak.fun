# lexical nearness never breaks a tie: two impls at the same argument are ambiguous
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; M = module { pub impl Size(I64) = module { size = fn(n) { 2 } } }; open M; Size.size(5) }
