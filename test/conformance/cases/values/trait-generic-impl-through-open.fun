# open delivers a module's generic impl, its own variables intact
{ trait Size(A) = sig { size : A -> I64 }; M = module { pub impl Size(Option(A)) = module { size = fn(o) { 2 } } }; open M; Size.size(Some(5)) }
