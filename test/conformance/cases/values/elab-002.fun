{ f = fn(x) { (module { pub a = 1; pub b = x }).b }; { _ = f(True); f(1) } }
