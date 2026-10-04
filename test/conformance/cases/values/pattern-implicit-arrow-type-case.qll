# `[a] -> b` matches an implicit Pi and names its binder; the codomain's mention
# of the name refers to it (the local rule), so a dependent codomain is writable
{ classify = fn(T : Type) {
    match (T) { [k] -> k -> k => 1, [k] -> Bool => 2, _ => 0 } };
  classify([k : Type] -> k -> k)
    + 10 * classify([k : Type] -> Bool)
    + 100 * classify(I64 -> Bool) }
