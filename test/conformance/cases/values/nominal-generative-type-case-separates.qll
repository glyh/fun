# E11: a type-case tells two evaluations of a generative module apart, by stamp
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(()); st2 = SymbolTable(());
  f = fn(t : Type) { match (t) { st1.Symbol => 1, _ => 0 } };
  f(st1.Symbol) * 10 + f(st2.Symbol) }
