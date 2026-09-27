# a generic impl's body uses its own variable's evidence: the impl takes that dictionary
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Option(A)) = module { size = fn(o) { match (o) { Some(x) => Size.size(x), None => 0 } } }; Size.size(Some(5)) }
