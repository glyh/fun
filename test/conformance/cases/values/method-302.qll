# method-signature-metas-capture-self: a parameter type built from a user type with implicit arguments
{ Pair = fn[A : Type, B : Type] { struct { fst : A; snd : B } };
  S = struct { k : I64; pub method first(p : Ref(Pair[I64, Bool])) ->{Mutate(p)} I64 { deref(p).fst } };
  s = S{ k = 1 }; q = ref(Pair[I64, Bool]{ fst = 4, snd = True }); s.first(q) }
