# type-case refinement reaches the evidence entries for a bound trait variable
{ trait Size(a) = sig { size : a -> I64 };
  f : [A : Size] -> A -> I64 = fn[A : Type](x) { match (A) { Char => Size.size(x) + 1, _ => 0 } };
  impl Size(Char) = module { size = fn(x) { 8 } };
  f('c') }
