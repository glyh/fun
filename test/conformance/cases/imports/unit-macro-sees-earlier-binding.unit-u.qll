open (import "std");
pub helper = fn(x : I64) { x };
pub macro five(_) { Syntax.i64(helper(5)) };
