# a module passed through a polymorphic function keeps its generic impl's own variables
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; Pick = fn[A : Type](x : A) { x }; W = fn(k) { Pick(module { pub impl s : Size(Option(a)) = module { size = fn(o) { 3 } } }) }; M = W(Unit); open M; Size.size(Some(5)) }
