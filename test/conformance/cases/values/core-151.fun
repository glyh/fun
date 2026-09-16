# method returns Self
{ Box = struct { value: I64; pub method copy() { self } }; (Box.copy(Box{value = 1})).value }
