# `a -> b` is an explicit, non-dependent function type: it matches neither an
# implicit Pi nor one whose result depends on its domain. A dependent *domain*
# with a constant codomain still matches.
{ classify = fn(T : Type) {
    match (T) { I64 -> Bool => 1, a -> b => 2, _ => 0 } };
  classify(I64 -> Bool)
    + 10 * classify(I64 -> I64)
    + 100 * classify(I64 -> I64 -> Bool)
    + 1000 * classify([k : Type] -> Bool)
    + 10000 * classify((k : Type) -> List(k))
    + 100000 * classify(((k : Type) -> List(k)) -> Bool) }
