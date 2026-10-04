{ macro m(s) { match (s) { Syntax.Expr.RawMatch(_, _, bs) => Syntax.i64(9), _ => Syntax.i64(0) } };
  m(match (1) { 1 => 2, _ => 3 }) }
