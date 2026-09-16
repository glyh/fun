{ bad : (T : Type) -> T -> Bool = fn(T : Type, x) { match (T) { I64 => x == False, _ => False } }; bad }
