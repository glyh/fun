# a pattern synonym over a sealed-nominal head: the head term runs under the definition site's closure
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  1 }
