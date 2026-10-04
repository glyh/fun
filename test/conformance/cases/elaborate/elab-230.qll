{ effect IO = sig { read : Unit -> I64 }; ((fn[r : EffectRow] { fn(_) { perform IO.read () } }) : [r : EffectRow] -> (Unit ->{IO | r} I64)) }
