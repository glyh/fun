{ mk = fn(B : Type) { rec L = struct { v : B; next : Option(L) }; L };
            LI = mk(I64); LI2 = mk(I64);
            x : LI = LI2{ v = 1, next = None }; y : LI = LI{ v = 2, next = Some(x) };
            match (y.next) { Some(z) => z.v, None => 0 } }
