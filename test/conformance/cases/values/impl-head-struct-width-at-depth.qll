# the rest is admitted at any depth: `Option(struct { a : q; _ })` matches a container of any record with field `a`
{ trait Size(a) = sig { size : a -> I64 };
  impl Size(I64) = module { size = fn(n) { n } };
  P = struct { a : I64 };
  R = struct { a : I64; b : Bool };
  impl Size(Option(struct { a : q; _ })) = module { size = fn(o) { match (o) { Some(x) => Size.size(x.a), None => 0 } } };
  Size.size(Some(P{a = 5})) * 10 + Size.size(Some(R{a = 6; b = True})) }
