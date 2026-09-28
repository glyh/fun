# an impl inside a module is not in scope until the module is opened
{ trait Size(a) = sig { size : a -> I64 }; M = module { pub impl Size(I64) = module { size = fn(x) { 4 } } }; f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) }; f(3) }
