# Trait.op resolves the impl by its argument type, not the innermost impl of the trait
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Char) = module { size = fn(c) { 2 } }; Size.size(5) }
