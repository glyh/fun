# ->{_} infers a method's row
{ effect Exc = sig { raise : I64 -> I64 }; C = struct { n : I64; pub method add(k : I64) ->{_} I64 { perform Exc.raise(k) } }; match (C.add(C{n = 1})(3)) { x => x, effect Exc.raise v => v + 10 } }
