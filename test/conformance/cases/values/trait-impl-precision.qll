# of two matching impls the more precise wins: Option(a) is an instance of a, not the reverse
{ trait Size(a) = sig { size : a -> I64 }; impl Size(a) = module { size = fn(x) { 1 } }; impl Size(Option(a)) = module { size = fn(o) { 2 } }; Size.size(Some(5)) }
