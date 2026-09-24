# an inline polymorphic lambda as an argument instantiates, as a name does
{ h = fn(g) { g[I64](7) }; h(fn[A : Type](a : A) { a }) }
