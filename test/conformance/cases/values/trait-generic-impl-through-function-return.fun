# a module returned by a function is re-evaluated: its generic impl keeps its own variables
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; Make = fn(u) { module { pub impl s : Size(Option(a)) = module { size = fn(o) { 3 } } } }; M = Make(Unit); open M; Size.size(Some(5)) }
