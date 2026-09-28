# an impl declared in a function promotes its own variable's bound: the body's demand
# for the element's evidence is a hidden dictionary the enclosing bound supplies
{ trait Size(a) = sig { size : a -> I64 }; impl Size(I64) = module { size = fn(n) { 7 } }; f : [B : {Size}] -> B -> I64 = fn[B : Type](x) { impl Size(List(b)) = module { size = fn(xs) { match (xs) { Nil => 0, Cons(h, t) => Size.size(h) } } }; Size.size(Cons[B](x, Nil[B])) }; f[I64](5) }
