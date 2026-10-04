# a method declaring its row, handled at the call
{ effect Exc = sig { raise : I64 -> I64 }; C = struct { n : I64; pub method bump() ->{Exc} I64 { perform Exc.raise(1); self.n } }; match (C.bump(C{n = 1})) { x => x, effect Exc.raise v => v + 10 } }
