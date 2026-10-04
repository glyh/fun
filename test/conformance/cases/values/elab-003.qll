{ k = 41; effect E = sig { tell : I64 -> I64 }; g = fn(x) { (fn(u) { x })(perform E.tell(k)) }; match (g(1)) { v => v, effect E.tell n => n } }
