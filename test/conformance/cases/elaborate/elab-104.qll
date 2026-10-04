{ rec L = fn(A : Type) { struct { v : A; next : Option(L(A)) } };
            g = fn(x : L(Bool)) { 1 }; g(L(I64){ v = 3, next = None }) }
