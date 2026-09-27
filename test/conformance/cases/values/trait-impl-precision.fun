# of two matching impls the more precise wins: Option(A) is an instance of A, not the reverse
{ trait Size(A) = sig { size : A -> I64 }; impl Size(A) = module { size = fn(x) { 1 } }; impl Size(Option(A)) = module { size = fn(o) { 2 } }; Size.size(Some(5)) }
