# each impl of a trait serves its own argument type: Size.size at Char uses Size(Char)
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Char) = module { size = fn(c) { 2 } }; (Size.size(5), Size.size('c')).1 }
