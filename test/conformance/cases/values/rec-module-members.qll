# recursive module members and groups push one entry each
{ M = module { pub rec loop = fn(n : I64) { loop(n) }; rec a = fn(n : I64) { b(n) } and b = fn(n : I64) { a(n) }; pub x = 3 }; M.x }
