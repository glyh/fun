# an impl method within its trait's row
{ effect Exc = sig { raise : I64 -> I64 }; effect Other = sig { ping : I64 -> I64 }; trait Log(A) = sig { log : A ->{Exc} I64 }; impl Log(I64) = module { log = fn(x) { perform Exc.raise(x) } }; 0 }
