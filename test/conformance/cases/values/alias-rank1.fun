# a ~> alias is rank 1: the definition taking it mints the row
{ effect Log = sig { write : I64 -> I64 }; Callback = Unit ~> I64; app = fn(g : Callback) ~> I64 { g() }; lg : Unit ->{Log} I64 = fn(u) { perform Log.write(7) }; match (app(lg)) { v => v, effect Log.write n => n * 6 } }
