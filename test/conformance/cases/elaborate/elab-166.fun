{ mk = fn(u : Unit) { module { table = ref(0); pub T = enum { Sym(I64) };
                                        pub make = fn(n : I64) { table <- n; T.Sym(n) } } };
            m1 = mk(()); m2 = mk(());
            f = fn(x : m2.T) { 1 }; f(m1.make(1)) }
