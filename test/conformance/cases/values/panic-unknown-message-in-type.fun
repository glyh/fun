# panic with a message not yet known stays stuck in a type; the function is never called
{ f = fn(s : String, x : panic[Type](s)) { x }; 1 }
