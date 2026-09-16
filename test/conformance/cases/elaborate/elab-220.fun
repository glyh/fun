{ effect Log = sig { write : I64 -> I64 }; f : [e : EffectRow] -> (I64 ->{Log | e} I64) -> I64 ->{Log | e} I64 = fn[e : EffectRow](g, x) { g(x) }; 1 }
