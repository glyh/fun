# a row binder written out in the annotation stays rank 2: a plain callback does not fit
{ effect Log = sig { write : I64 -> I64 }; Poly = [e : EffectRow] -> Unit ->{e} I64; app = fn(g : Poly) ~> I64 { g() }; lg : Unit ->{Log} I64 = fn(u) { perform Log.write(1) }; app(lg) }
