open (import "std");
pub trait Size(A) = sig { size : A -> I64 };
pub impl Size(I64) = module { size = fn(n) { 6 } };
pub M = module { pub impl Size(Option(A)) = module { size = fn(o) { match (o) { Some(x) => Size.size(x), None => 0 } } } };
