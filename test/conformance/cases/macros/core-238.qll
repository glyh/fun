# 7G: ADT matching preserves binding hygiene
{ x = 1; macro passthrough(stx) { match (stx) { Syntax.Lam(_, _) => stx, _ => stx } }; (passthrough(fn(x) { x }))(42) }
