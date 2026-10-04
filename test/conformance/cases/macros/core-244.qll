# 7G: DEBUG inner match with named binders
{ macro t(stx) { match (stx) { Syntax.Ap(inner, _) => match (inner) { Syntax.Ap(f, a) => Syntax.i64(1), _ => Syntax.i64(0) }, _ => Syntax.i64(0) } }; result = t((fn(x, y) { x - y })(5, 3)); result }
