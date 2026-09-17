# a match on an unknown value in a type waits until the value is known
{ f = fn(b : Bool, x : match (b) { True => I64, False => Char }) { x }; f(True, 5) }
