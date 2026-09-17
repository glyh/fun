# an impl choice waits for its argument type: x is only known to be I64 at the call
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 7 } }; (fn(x) { Size.size(x) })(5) }
