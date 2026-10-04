# a parameterised effect instance tunnels
{ effect E(A) = sig { raise : I64 -> I64 }; find : [r : EffectRow] -> (I64 ->{| r} I64) -> I64 ->{| r} I64 = fn[r : EffectRow](pred, x) { match (pred(x)) { v => v, effect E.raise n => 0 } }; user : I64 ->{E(I64)} I64 = fn(x) { perform E.raise(x) }; match (find(user, 1)) { v => v, effect E.raise n => 999 } }
