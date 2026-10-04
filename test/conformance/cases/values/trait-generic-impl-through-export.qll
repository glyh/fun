# export carries a generic impl's own variables, as open does
{ trait Size(a) = sig { size : a -> I64 }; M = module { pub impl s : Size(Option(a)) = module { size = fn(o) { 3 } } }; E = module { export M }; open E; Size.size(Some(5)) }
