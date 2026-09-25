# a pattern synonym over a sealed-nominal head: the match sits one binder deeper than the
# definition, and the head term must still resolve where it was written (it was value 42 only
# at top level before the level-base fix; the prototype still fails here - see the divergence list)
{ SymbolTable = fn(u : Unit) { module { table = ref(""); pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  f = fn(x : Type) { match (st1.Symbol : Type) { M.S => 42, _ => 0 } };
  f(I64) }
