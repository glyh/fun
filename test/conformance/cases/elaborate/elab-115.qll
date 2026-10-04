{ type Box A = MkBox(A); BoolBox = Box(Bool); f = fn(b : Box(I64)) { b }; g = fn(b : Box(I64)) { fn(u : Unit) { y : Box(I64) = f(b); y } }; 1 }
