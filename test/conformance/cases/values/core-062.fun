# a closure scrutinee under a handler
{ effect Exc = sig { raise : I64 -> I64 }; h = match (fn(u : Unit) { 1 }) { x => x, effect Exc.raise n => fn(u : Unit) { n } }; h(()) }
