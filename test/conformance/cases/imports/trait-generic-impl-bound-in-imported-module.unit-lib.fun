open (import "std");
pub trait Size(a) = sig { size : a -> I64 };
pub impl Size(I64) = module { size = fn(n) { 6 } };
pub M = module { pub impl Size(Option(a)) = module { size = fn(o) { match (o) { Some(x) => Size.size(x), None => 0 } } } };
