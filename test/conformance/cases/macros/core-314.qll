# an Id parameter names the use site's binder
{ macro same(n : Id) { Syntax.RawVar(None, n) }; x = 5; same(x) }
