# the same, two binders deeper: the shift is a level difference, not a fixed one
{ SymbolTable = fn(u : Unit) { module { table = ref(""); pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  f = fn(x : Type, y : Type) { match (st1.Symbol : Type) { M.S => 42, _ => 0 } };
  f(I64, Bool) }
