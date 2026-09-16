# the same body is fine once the result says what it performs
{ effect Log = sig { write : I64 -> I64 }; good = fn(u : Unit) ->{Log} I64 { perform Log.write(1) }; match (good(())) { v => v, effect Log.write n => n + 4 } }
