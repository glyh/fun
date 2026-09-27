# export carries a bound-carrying generic impl, and its dictionary resolves at the use
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 5 } }; M = module { pub impl s : Size(Option(A)) = module { size = fn(o) { match (o) { Some(x) => Size.size(x), None => 0 } } } }; E = module { export M }; open E; Size.size(Some(5)) }
