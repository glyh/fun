# open M.{i64_size} opens the named impl; the trait reached by qualifying M.Size
{ M = module { pub trait Size(a) = sig { size : a -> I64 };
              pub impl i64_size : Size(I64) = module { size = fn(n) { 5 } } };
  open M.{i64_size}; M.Size.size(5) }
