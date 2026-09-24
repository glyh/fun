# a rec enum former captures what its own body names, not a name the outer body names elsewhere: n is nowhere in fn(A : Type) { enum { X(A) } }, so it is not a free variable of the declaration and must not split the type (E11)
{ F = fn(n : I64) { y = n; rec T = fn(A : Type) { enum { X(A) } }; T }; a = F(1); b = F(2); take = fn(z : a(I64)) { 1 }; take(b(I64).X(3)) }
