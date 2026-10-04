# a pin in an impl head names the alias it is written with: `Option(^z)` with `z = I64`
{ trait Size(a) = sig { size : a -> I64 };
  impl Size(I64) = module { size = fn(n) { 1 } };
  z = I64;
  impl Size(Option(^z)) = module { size = fn(o) { 2 } };
  Size.size(Some(5)) + Size.size(9) }
