# E11: a generative module's type is unique per evaluation, and usable with itself
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) };
    pub name = fn(x : Symbol) { match (x) { Sym(n) => n } } } };
  st1 = SymbolTable(()); st2 = SymbolTable(());
  g = fn(x : st1.Symbol) { 1 };
  g(st1.intern("x")) }
