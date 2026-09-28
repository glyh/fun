# equality still sees a struct's methods, and compares their signatures: these two do not unify
{ P = struct { a : I64; pub method m() : I64 { self.a } };
  U = struct { a : I64; pub method m(x : I64) : I64 { x } };
  f = fn(o : P) : I64 { o.a };
  f(U{a = 1}) }
