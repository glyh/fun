# parameter-type-metas-capture-earlier-parameters
{ f = fn(a : I64, r : Ref(I64), s : Ref(I64)) : I64 { a };
  x = ref(0);
  y = ref(1);
  f(0, x, y) }
