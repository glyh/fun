{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
            less = fn(a : I64, b : I64) { a < b }; greater = fn(a : I64, b : I64) { a > b };
            a = Set(I64, less); b = Set(I64, less); c = Set(I64, greater);
            f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } };
            f(b.T) * 10 + f(c.T) }
