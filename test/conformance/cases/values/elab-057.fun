{ less = fn(a : I64, b : I64) { a < b };
            Set = fn(Elem : Type, cmp : I64 -> I64 -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub single = fn(x : Elem) { Node(Leaf, x, Leaf) };
              pub union = fn(a : T, b : T) { a };
              pub size = fn(t : T) { match (t) { Leaf => 0, Node(_, _, _) => 1 } } } };
            a = Set(I64, less); b = Set(I64, less);
            a.size(a.union(a.single(1), b.single(2))) }
