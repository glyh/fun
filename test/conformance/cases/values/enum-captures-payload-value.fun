# an enum's captures come from its payload values: A is transparent (A = X), so C's
# payload sees X itself - applied at I64 and matched, so the case observes an I64
# rather than printing the former
{ f = fn(X : Type) { A = X; rec U = enum { C(A) }; fn(x : X) { match (U.C(x)) { U.C(n) => n } } }; f(I64)(3) }
