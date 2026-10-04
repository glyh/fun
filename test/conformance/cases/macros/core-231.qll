# Syntax identifier inspection
{ macro inspect(stx) { match (stx) { Syntax.Var(_) => Syntax.i64(1), _ => Syntax.i64(0) } }; target = 10; inspect(target) }
