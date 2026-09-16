# a sealed generative type may not leave the scope of the binder that names it (E11)
{ mk = fn(u : Unit) { module { table = ref(0); pub T = enum { Sym(I64) }; pub make = fn(n : I64) { table <- n; T.Sym(n) } } }; x = { m = mk(()); m.make(1) }; 1 }
