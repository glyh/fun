# E6: a handled effect's closure may not leave the handler inside a module
{ effect Exc = sig { raise : I64 -> I64 }; g = match (0) { x => module { pub f = fn(u : Unit) { perform Exc.raise(x) } }, effect Exc.raise n => module { pub f = fn(u : Unit) { perform Exc.raise(n) } } }; 1 }
