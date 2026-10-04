# parameter-type-metas-capture-earlier-parameters
# An implicit type parameter mentioned by the following written type.
{ f = fn[A : Type](r : Ref(A)) : I64 { 0 }; x = ref(0); f(x) }
