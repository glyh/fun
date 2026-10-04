# an effect the checker's own evaluation performs with no handler in scope is an error naming it
{ effect Abort = sig { stop : Unit -> I64 };
  f = fn(u : Unit) { perform Abort.stop(u); I64 };
  E = enum { C(f(())) };
  1 }