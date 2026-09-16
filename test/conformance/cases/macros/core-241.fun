# 7G: simple swap args via nested match
{ macro swap(stx) { match (stx) { Syntax.Ap(inner, b) => match (inner) { Syntax.Ap(f, a) => Syntax.ap(Syntax.ap(f, b), a), _ => stx }, _ => stx } }; result = swap((fn(x, y) { x - y })(5, 3)); result }
