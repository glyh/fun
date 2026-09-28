# a bounded function receives the impl in scope as a hidden dictionary
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(x) { 8 } }; f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) }; f(3) }
