# an impl head's free name is its own type variable: Option(a) serves Option(I64)
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Char) = module { size = fn(c) { 2 } }; impl Size(Option(a)) = module { size = fn(o) { 3 } }; Size.size(Some(5)) }
