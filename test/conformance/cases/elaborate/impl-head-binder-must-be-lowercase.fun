# an impl head's free name is a binder, so it must be lowercase: `A` here would bind if it were `a`
{ trait Size(a) = sig { size : a -> I64 }; impl Size(Option(A)) = module { size = fn(o) { 0 } }; 0 }
