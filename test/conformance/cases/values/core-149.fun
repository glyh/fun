# method extra parameter
{ Counter = struct { value: I64; pub method add(x) { self.value + x } }; Counter.add(Counter{value = 1})(2) }
