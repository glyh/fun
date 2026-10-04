# a handled perform and an uncalled effectful function
{ effect Exc = sig { raise : I64 -> I64 };
       f = fn(u : Unit) { perform Exc.raise(1) };
       match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n + 1 } }
