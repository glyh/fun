# Syntax.kind expression object
{ macro answer(stx) { match (stx) { Syntax.Var(_) => Syntax.i64(1), _ => Syntax.i64(0) } }; x = 10; answer(x) }
