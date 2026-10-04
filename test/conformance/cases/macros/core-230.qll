# Syntax ap deconstructors
{ macro f(stx) { match (stx) { Syntax.Ap(f, a) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(add(1, 2)) }
