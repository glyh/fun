{ effect Exc = sig { raise : I64 -> I64 }; (fn(_) { match (perform Exc.raise(1)) { x => x, effect Exc.raise n => resume(n + 1) } } : Unit -> I64) }
