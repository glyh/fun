open (import "std"); pub macro same(e) { quote((fn(y) { y })($e)) }
