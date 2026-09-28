# a selective open twice, then wholesale, delivers one impl by identity
{ trait Size(a) = sig { size : a -> I64 };
  M = module { pub impl s : Size(Option(a)) = module { size = fn(o) { 3 } } };
  open M.{s}; open M.{s}; open M;
  f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) };
  f(Some(5)) }
