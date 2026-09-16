# handler lexical resume nested lambda
{ effect Exc = sig { raise : I64 -> I64 };
     match (perform Exc.raise(1)) { x => x,
     effect Exc.raise n => (fn(x) { resume(x + 40) })(n)
     }
     }
