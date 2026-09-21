# E11: a symbol may not cross tables - st2.name takes st2.Symbol
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) };
    pub name = fn(x : Symbol) { match (x) { Sym(n) => n } } } };
  st1 = SymbolTable(()); st2 = SymbolTable(());
  st2.name(st1.intern("x")) }
