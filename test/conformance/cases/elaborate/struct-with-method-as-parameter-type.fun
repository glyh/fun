# a struct with a public method is still a valid written parameter type
{ S = struct { k : I64; pub method m() : I64 { self.k } };
  f = fn(o : S) : I64 { o.k };
  s = S{ k = 7 }; f(s) }
