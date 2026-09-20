# E11: st2.Symbol is a different type from st1.Symbol
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(()); st2 = SymbolTable(());
  g = fn(x : st1.Symbol) { 1 };
  g(st2.intern("x")) }
