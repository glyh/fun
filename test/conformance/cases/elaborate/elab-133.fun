{ C = struct { v : I64; pub method get() { self.v } }; D = struct { v : I64 }; f = fn(x : D) { x.v }; f(C{v = 1}) }
