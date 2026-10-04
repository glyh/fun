# a method with no declared row is pure (E3): its body may not perform
{ effect Exc = sig { raise : I64 -> I64 }; C = struct { n : I64; pub method bump() { perform Exc.raise(1) } }; 1 }
