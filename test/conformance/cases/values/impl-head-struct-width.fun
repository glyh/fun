# a width head `struct { a : p; _ }` matches any record having field `a`, and binds `p`
{ trait Size(a) = sig { size : a -> I64 };
  impl Size(I64) = module { size = fn(n) { n } };
  P = struct { a : I64 };
  R = struct { a : I64; b : Bool };
  U = struct { a : I64; pub method m() : I64 { self.a } };
  impl Size(struct { a : p; _ }) = module { size = fn(x) { Size.size(x.a) } };
  Size.size(P{a = 1}) * 100 + Size.size(R{a = 2; b = True}) * 10 + Size.size(U{a = 3}) }
