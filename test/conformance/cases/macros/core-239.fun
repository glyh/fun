# 7G: rebuilt lambda binds its body through its own parameter
{ macro double(stx) { match (stx) { Syntax.Lam(p, body) => Syntax.RawLam(None, p, quote($body + $body)), _ => Syntax.i64(0) } }; (double(fn(x) { x }))(40) }
