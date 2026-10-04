{ C = struct { v : I64; pub method b() : I64 { self.later(5) }; pub method later(k : I64) : I64 { self.v * k } }; C{v = 2}.b() }
