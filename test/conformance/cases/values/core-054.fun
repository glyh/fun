# an impl method within its trait's row
{ effect Exc = sig { raise : I64 -> I64 }; effect Other = sig { ping : I64 -> I64 }; trait Log(a) = sig { log : a ->{Exc} I64 }; impl Log(I64) = module { log = fn(x) { perform Exc.raise(x) } }; 0 }
