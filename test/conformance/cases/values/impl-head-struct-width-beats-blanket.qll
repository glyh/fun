# a width head is more precise than the blanket `_`, which stays last
{ trait Size(a) = sig { size : a -> I64 };
  R = struct { a : I64; b : Bool };
  impl Size(struct { a : p; _ }) = module { size = fn(x) { 7 } };
  impl Size(_) = module { size = fn(x) { 0 } };
  Size.size(R{a = 1; b = True}) + Size.size(5) }
