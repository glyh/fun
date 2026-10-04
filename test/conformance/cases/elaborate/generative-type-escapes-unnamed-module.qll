# a member of a generative module no binder names may not mention a type the module declares (E11)
{ mk = fn(u : Unit) { module { table = ref(0); pub T = enum { Sym(I64) }; pub make = fn(n : I64) { table <- n; T.Sym(n) } } }; x = mk(()).make(1); 1 }
