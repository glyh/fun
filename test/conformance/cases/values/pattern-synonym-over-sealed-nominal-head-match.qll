# the same synonym used in a match: the direct-match path re-evaluates the carried head term
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  match (st1.Symbol : Type) { M.S => 42, _ => 0 } }
