# every member of a rec … and … group sees every member
{ rec a = fn(n : I64) { b(n) } and b = fn(n : I64) { a(n) }; 2 }
