# an extra *field* is not width: the head still does not match, and the blanket answers
{ trait Size(a) = sig { size : a -> I64 };
  R = struct { a : I64; b : Bool };
  impl Size(struct { a : I64 }) = module { size = fn(x) { 1 } };
  impl Size(_) = module { size = fn(x) { 0 } };
  Size.size(R{a = 1; b = True}) }
