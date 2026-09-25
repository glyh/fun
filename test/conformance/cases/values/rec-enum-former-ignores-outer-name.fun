# E11: a nominal captures its own free variables plus enclosing module stamps — an
# unused enclosing binding must not split the type (applicativity, footgun 6)
{ F = fn(n : I64) { y = n; rec T = fn(A : Type) { enum { X(A) } }; T };
  a = F(1); b = F(2); take = fn(z : a(I64)) { 1 }; take(b(I64).X(3)) }
