{ type Symbol = Sym(I64);
            mk = fn(u : Unit) { module { table = ref(0); pub T = Symbol; pub wrap = fn(n : I64) { table <- n; Sym(n) } } };
            m = mk(());
            f = fn(s : Symbol) { match (s) { Sym(n) => n } };
            f(m.wrap(4)) }
