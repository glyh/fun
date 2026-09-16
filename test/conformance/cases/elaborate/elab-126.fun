{ effect Exc = sig { raise : I64 -> I64 }; C = struct { v : I64; pub method bump() ->{Exc} I64 { perform Exc.raise(1) } }; C.bump }
