# a call whose row names the effect is handled locally
{ effect Exc = sig { raise : I64 -> I64 }; helper : Unit ->{Exc} I64 = fn(_) { perform Exc.raise(5) }; match (helper(())) { v => v, effect Exc.raise n => n + 1 } }
