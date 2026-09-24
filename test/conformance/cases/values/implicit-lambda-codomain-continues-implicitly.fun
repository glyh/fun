# an implicit lambda whose expected codomain continues implicitly binds its parameters itself
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(x) { 8 } }; g : [A : Type] -> [B : Size] -> B -> I64 = fn[A : Type, B : Type](b : B) { Size.size(b) }; g(3) }
