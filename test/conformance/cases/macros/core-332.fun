{ macro m(s) { Syntax.Expr.RawMatch(None, s, Cons(Syntax.ValueBranch(Syntax.pat_wild, Syntax.i64(7)), Nil)) };
  m(3) }
