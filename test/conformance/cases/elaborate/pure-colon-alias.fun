# a ~> alias under a pure result is rejected: the callback performs
{ Callback = Unit ~> I64; app = fn(g : Callback) : I64 { g() }; 1 }
