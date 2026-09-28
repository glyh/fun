# two head variables, two bounds: each dictionary is threaded to the occurrence that names it
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Char) = module { size = fn(c) { 2 } }; impl Size(Tuple(2, a, b)) = module { size = fn(p) { match (p) { (x, y) => Size.size(x) + Size.size(y) } } }; Size.size((1, 'c')) }
