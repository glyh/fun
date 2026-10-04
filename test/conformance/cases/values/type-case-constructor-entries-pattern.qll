# a constructor pattern resolves through entries a type-case branch refines
({ Opt = fn(A : Type) { enum { Some2(A), None2 } };
   f : [T : Type] -> Opt(T) -> I64 = fn[T](o) {
     open Opt(T);
     match (T) { I64 => (match (o) { Some2(n) => n + 1, None2 => 0 }), _ => 0 } };
   f(Opt(I64).Some2(7)) })
