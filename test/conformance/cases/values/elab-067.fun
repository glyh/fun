{ SymbolTable = fn(u : Unit) { module {
       table = ref(0);
       pub type Symbol = Sym(I64);
       pub intern = fn(s : I64) { table <- deref(table) + s; Sym(deref(table)) };
       pub name = fn(x : Symbol) { match (x) { Sym(n) => n } } } };
     st1 = SymbolTable(()); st2 = SymbolTable(()); f = fn(t : Type) { match (t) { st1.Symbol => 1, _ => 0 } }; f(st1.Symbol) * 10 + f(st2.Symbol) }
