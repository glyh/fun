# two incomparable width heads at a use matching both: ambiguous implementation
{ trait Size(a) = sig { size : a -> I64 };
  R = struct { a : I64; b : Bool };
  impl Size(struct { a : I64; _ }) = module { size = fn(x) { 1 } };
  impl Size(struct { b : Bool; _ }) = module { size = fn(x) { 2 } };
  Size.size(R{a = 1; b = True}) }
