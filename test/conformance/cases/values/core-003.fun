# apply twice
{
                  twice : (I64 -> I64) -> I64 -> I64 = fn(f) { fn(x) { f(f(x)) } };
                  inc : I64 -> I64 = fn(n) { n + 1 };
                  twice(inc, 3)
                }
