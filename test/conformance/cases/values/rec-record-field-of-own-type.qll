# a recursive record field holds a recursive occurrence, which is a type
{ rec L = struct { v : I64; next : L }; f = fn(l : L) { l.v }; 1 }
