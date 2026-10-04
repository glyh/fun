{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub single = fn(x : Elem) : T { Node(Leaf, x, Leaf) };
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) };
              pub union = fn(a : T, b : T) : T { a } } };
            less = fn(a : I64, b : I64) { a < b }; greater = fn(a : I64, b : I64) { a > b };
            up = Set(I64, less); down = Set(I64, greater);
            up.union(up.single(1), down.single(2)) }
