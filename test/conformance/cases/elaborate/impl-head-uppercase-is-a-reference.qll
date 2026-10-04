# a bare uppercase name refers; one that resolves to nothing in an impl head is an error, not a fresh variable
{ trait Size(a) = sig { size : a -> I64 }; impl Size(A) = module { size = fn(x) { 1 } }; 0 }
