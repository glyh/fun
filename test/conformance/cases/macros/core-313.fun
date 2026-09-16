# the same impl through two paths counts once
{ M = module { pub rec C = enum { K }; pub impl eq_C : Eq(C) = module { eq = fn(x, y) { True } } };
       N = module { export M };
       open M; open N; if (M.C.K == M.C.K) { 1 } else { 0 } }
