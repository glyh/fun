# 7G: computed multi-kind dispatch not possible with templates
{ macro classify(stx) { match (stx) { Syntax.Lam(_, _) => Syntax.i64(1), Syntax.Ap(_, _) => Syntax.i64(2), Syntax.Var(_) => Syntax.i64(3), _ => Syntax.i64(0) } }; classify(fn(x) { x }) }
