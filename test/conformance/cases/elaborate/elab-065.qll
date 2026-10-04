{ SymbolTable = fn(u : Unit) { module {
       table = ref(0);
       pub type Symbol = Sym(I64);
       pub intern = fn(s : I64) { table <- deref(table) + s; Sym(deref(table)) };
       pub name = fn(x : Symbol) { match (x) { Sym(n) => n } } } };
     st1 = SymbolTable(()); st2 = SymbolTable(()); st2.name(st1.intern(5)) }
