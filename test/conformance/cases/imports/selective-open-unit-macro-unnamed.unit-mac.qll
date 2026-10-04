open (import "std");
pub macro same(e) { quote((fn(y) { y })($e)) };
pub macro other(e) { quote($e) }
