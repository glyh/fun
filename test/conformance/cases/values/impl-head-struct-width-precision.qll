# two width heads are ordered by precision: `struct { a : p; b : q; _ }` beats `struct { a : p; _ }`
{ trait Size(a) = sig { size : a -> I64 };
  R = struct { a : I64; b : Bool };
  P = struct { a : I64 };
  impl Size(struct { a : p; _ }) = module { size = fn(x) { 1 } };
  impl Size(struct { a : p; b : q; _ }) = module { size = fn(x) { 2 } };
  Size.size(R{a = 1; b = True}) * 10 + Size.size(P{a = 1}) }
