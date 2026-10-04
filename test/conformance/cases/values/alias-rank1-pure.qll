# a ~> alias takes a pure callback too
{ Callback = Unit ~> I64; app = fn(g : Callback) ~> I64 { g() }; app(fn(u : Unit) { 9 }) }
