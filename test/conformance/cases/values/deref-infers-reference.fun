# reading a value of unknown type infers that it is a reference
{ f = fn(r) { deref(r) }; f(ref 7) }
