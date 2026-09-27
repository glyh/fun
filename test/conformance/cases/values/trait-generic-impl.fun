# an impl head's free name is its own type variable: Option(A) serves Option(I64)
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Char) = module { size = fn(c) { 2 } }; impl Size(Option(A)) = module { size = fn(o) { 3 } }; Size.size(Some(5)) }
