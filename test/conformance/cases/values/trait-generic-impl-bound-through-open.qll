# open delivers a generic impl that draws on its own variable's bound
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 4 } }; M = module { pub impl Size(Option(a)) = module { size = fn(o) { match (o) { Some(x) => Size.size(x), None => 0 } } } }; open M; Size.size(Some(5)) }
