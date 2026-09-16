# two different impls for one trait and argument in scope are ambiguous
{ trait Size(A) = sig { size : A -> I64 }; M = module { pub impl Size(I64) = module { size = fn(x) { 1 } } }; N = module { pub impl Size(I64) = module { size = fn(x) { 2 } } }; open M; open N; f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) }; f(3) }
