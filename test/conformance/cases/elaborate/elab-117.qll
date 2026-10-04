{ Box = struct { value: I64; pub id = fn(b : Self) { b.value } }; Box.id(Box{value = 1}) }
