# the nominal-payload variant: T is a nominal capturing X, and U's payload value
# mentions it - so U captures X; observed through a match on U's constructor
{ f = fn(X : Type) { T = enum { K(X) }; rec U = enum { C(T) }; fn(x : X) { match (U.C(T.K(x))) { U.C(T.K(n)) => n } } }; f(I64)(3) }
