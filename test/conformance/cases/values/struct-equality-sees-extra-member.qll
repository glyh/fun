# equality is unchanged: it sees every shown member, so the record does not take the plain struct type
{ P = struct { a : I64 };
  U = struct { a : I64; pub method m() : I64 { self.a } };
  x : P = U{a = 1};
  x.a }
