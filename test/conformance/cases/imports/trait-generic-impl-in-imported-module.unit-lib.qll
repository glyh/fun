open (import "std");
pub trait Size(a) = sig { size : a -> I64 };
pub M = module { pub impl Size(Option(a)) = module { size = fn(o) { 6 } } };
