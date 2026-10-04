{ SymbolTable = fn(u : Unit) { module {
       table = ref(0);
       pub type Symbol = Sym(I64);
       pub intern = fn(s : I64) { table <- deref(table) + s; Sym(deref(table)) };
       pub name = fn(x : Symbol) { match (x) { Sym(n) => n } } } };
     st1 = SymbolTable(()); st2 = SymbolTable(()); g2 = fn(x : st2.Symbol) { 1 }; g2(st1.intern(5)) }
