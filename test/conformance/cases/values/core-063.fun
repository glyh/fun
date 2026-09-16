# handler outer bubble
{ effect Exc = sig { raise : I64 -> I64 }; match (match (perform Exc.raise(1)) { x => x }) { x => x, effect Exc.raise n => n + 1 } }
