# the inferred arrow is implicit: only an implicit function accepts the poly argument
{ h = fn(g) { g[I64]; 7 }; h(fn[A : Type](a : A) { a }) }
