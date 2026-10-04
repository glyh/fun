# a field-naming impl head matches a record whose extra member is a public method
{ trait Size(a) = sig { size : a -> I64 };
  U = struct { a : I64; pub method m() : I64 { self.a } };
  impl Size(struct { a : I64 }) = module { size = fn(x) { 1 } };
  impl Size(_) = module { size = fn(x) { 0 } };
  Size.size(U{a = 1}) }
