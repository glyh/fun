# a duplicate effect is kept once when an effect row is normalised
{ effect Log = sig { write : I64 -> I64 }; both = fn(f : I64 ~> I64, g : I64 ~> I64) ~> I64 { f(1) + g(2) }; consume = fn(k : (I64 ->{Log} I64) -> (I64 ->{Log} I64) ->{Log} I64) ->{Log} I64 { 0 }; match (consume(both)) { r => r, effect Log.write n => n } }
