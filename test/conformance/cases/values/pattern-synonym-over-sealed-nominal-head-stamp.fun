# a second evaluation's same-named nominal is another instance: the stamp survives the
# re-evaluation, so the two are not merged
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  st2 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  match (st2.Symbol : Type) { M.S => 42, _ => 0 } }
