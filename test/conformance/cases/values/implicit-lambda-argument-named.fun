# the same lambda bound to a name first - let-inlining must not change it
{ ch = fn[A : Type](a : A) { a }; h = fn(g) { g[I64](7) }; h(ch) }
