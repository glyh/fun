{ F = fn(n : I64) { if (n == 0) { I64 } else { Bool } };
            mk = fn(n : I64) { module { pub type T = A(F(n)) | B } };
            f = fn(t : mk(0).T) { 1 }; f(mk(1).B) }
