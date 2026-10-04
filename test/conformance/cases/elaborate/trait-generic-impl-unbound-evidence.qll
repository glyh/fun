# a generic impl's body cannot use evidence its head does not bind: missing at the definition
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Option(a)) = module { size = fn(o) { Size.size(o) } }; 0 }
