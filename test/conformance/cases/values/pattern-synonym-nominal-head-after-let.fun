# and one let-binding deeper, so it is the binder count that moves, not the shape
{ SymbolTable = fn(u : Unit) { module { table = ref(""); pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  { k = 7; match (st1.Symbol : Type) { M.S => 42, _ => 0 } } }
