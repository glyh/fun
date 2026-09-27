# parameter-type-metas-capture-earlier-parameters
# The control: the argument is a lambda parameter, so it was always a variable.
{ f = fn(a : I64, r : Ref(I64)) : I64 { a };
  g = fn(y : I64) { x = ref(40); f(y, x) };
  g(0) }
