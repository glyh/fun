# a field-naming impl head matches a record whose extra member is a public impl
{ trait Size(a) = sig { size : a -> I64 };
  U = struct { a : I64; pub impl Eq(Self) = module { fn eq(lhs, rhs) { lhs.a == rhs.a } } };
  impl Size(struct { a : I64 }) = module { size = fn(x) { 1 } };
  impl Size(_) = module { size = fn(x) { 0 } };
  Size.size(U{a = 1}) }
