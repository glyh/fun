# a local ref may hold a handled closure
{ effect Exc = sig { raise : I64 -> I64 }; match (0) { x => { q = ref(fn(u : Unit) { perform Exc.raise(x) }); 1 }, effect Exc.raise n => 2 } }
