# : T on a definition means a pure result: an effectful body must use the arrow form
{ effect Log = sig { write : I64 -> I64 }; bad = fn(u : Unit) : I64 { perform Log.write(1) }; 1 }
