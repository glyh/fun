open (import "std");
pub macro five(_) { Syntax.i64(helper(5)) };
pub helper = fn(x : I64) { x };
