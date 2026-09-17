open (import "std"); macro same(e) { quote((fn(y) { y })($e)) }; pub x = 1
