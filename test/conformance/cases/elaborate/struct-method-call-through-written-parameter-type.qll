# the method's signature is reachable through the written parameter type, and the call runs its definition
{ S = struct { k : I64; pub method m() : I64 { self.k } };
  f = fn(o : S) : I64 { o.k + o.m() };
  f(S{k = 1}) }
