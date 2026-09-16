# two recursive records with the same shape are different types: an occurrence is equal only to one of its own declaration
{ rec L = struct { v : I64; next : L }; rec K = struct { v : I64; next : K }; f = fn(l : L) { l }; g = fn(k : K) { f(k) }; 1 }
