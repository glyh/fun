# export carries a generic impl's own variables, as open does
{ trait Size(A) = sig { size : A -> I64 }; M = module { pub impl s : Size(Option(A)) = module { size = fn(o) { 3 } } }; E = module { export M }; open E; Size.size(Some(5)) }
