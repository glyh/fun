# a closure performing a handled effect may not escape through a reference outside the match (E6)
{ effect Exc = sig { raise : I64 -> I64 }; q = ref(fn(u : Unit) ->{Exc} I64 { 0 }); match (0) { x => { _ = q <- fn(u : Unit) ->{Exc} I64 { perform Exc.raise(x) }; 1 }, effect Exc.raise n => 2 } }
