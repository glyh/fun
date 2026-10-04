# 7G: DEBUG outer match with named binders
{ macro t(stx) { match (stx) { Syntax.Ap(inner, b) => Syntax.i64(1), _ => Syntax.i64(0) } }; result = t((fn(x, y) { x - y })(5, 3)); result }
