# a method row naming a ref from outside the struct, called
{ q = ref(40); S = struct { k : I64; pub method bump() ->{Mutate(q)} I64 { q <- deref(q) + self.k; deref(q) } }; s = S{ k = 2 }; s.bump() }
