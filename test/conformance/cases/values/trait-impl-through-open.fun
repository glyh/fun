# open delivers a module's public impls
{ trait Size(A) = sig { size : A -> I64 }; M = module { pub impl Size(I64) = module { size = fn(x) { 4 } } }; open M; f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) }; f(3) }
