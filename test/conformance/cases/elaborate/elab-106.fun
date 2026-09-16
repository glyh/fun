{ mk = fn(B : Type) { rec L = struct { v : B; next : Option(L) }; L };
            LI = mk(I64); LB = mk(Bool); x : LI = LI{ v = 1, next = None }; y : LB = x; 1 }
