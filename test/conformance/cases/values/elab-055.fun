{ mk = fn(u : Unit) { module { pub type T = A | B } }; m1 = mk(()); f = fn(x : m1.T) { 1 }; f(m1.A) }
