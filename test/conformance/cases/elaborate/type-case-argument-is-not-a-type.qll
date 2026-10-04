# an applied type-case argument that names no type is refused, naming the argument at fault
{ classify = fn(T : Type) { match (T) { Option(Some(1)) => 1, _ => 0 } }; classify(Option(I64)) }
