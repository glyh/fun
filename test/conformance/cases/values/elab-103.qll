{ rec L = fn(A : Type) { struct { v : A; next : Option(L(A)) } };
            f = fn(x : L(I64)) { x.v }; f(L(I64){ v = 3, next = None }) }
