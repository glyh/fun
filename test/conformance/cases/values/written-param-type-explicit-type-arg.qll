# parameter-type-metas-capture-earlier-parameters
{ f = fn[A : Type](a : A, r : Ref(I64)) : A { a };
  x = ref(40);
  f[I64](1, x) }
