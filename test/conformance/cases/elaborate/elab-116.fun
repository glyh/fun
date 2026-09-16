{ OB = Option(Bool); f = fn(o : Option(I64)) { o }; g = fn(o : Option(I64)) { y : Option(I64) = f(o); y }; 1 }
