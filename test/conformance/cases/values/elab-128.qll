{ C = struct { v : I64; pub method a(k : I64) { self.v + k }; pub method b() { self.a(2) } }; C.b(C{v = 1}) }
