open (import "std");
pub trait Size(A) = sig { size : A -> I64 };
pub impl Size(Option(A)) = module { size = fn(o) { 5 } };
