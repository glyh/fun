# open delivers a module's generic impl, its own variables intact
{ trait Size(a) = sig { size : a -> I64 }; M = module { pub impl Size(Option(a)) = module { size = fn(o) { 2 } } }; open M; Size.size(Some(5)) }
