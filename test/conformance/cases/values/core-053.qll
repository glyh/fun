# a method calling another performs its declared row
{ effect Exc = sig { raise : I64 -> I64 }; C = struct { n : I64; pub method a(k : I64) ->{Exc} I64 { perform Exc.raise(k) }; pub method b() ->{Exc} I64 { a(self)(2) } }; match (C.b(C{n = 1})) { x => x, effect Exc.raise v => v + 10 } }
