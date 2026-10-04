# a record pattern head must name a struct, not a value of unknown type
{ f = fn(P) { match (P) { P {x} => x } }; 1 }
