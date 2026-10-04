# naming the trait too is what brings it into scope
{ M = module { pub trait Size(a) = sig { size : a -> I64 };
              pub impl i64_size : Size(I64) = module { size = fn(n) { 5 } } };
  open M.{Size, i64_size}; Size.size(5) }
