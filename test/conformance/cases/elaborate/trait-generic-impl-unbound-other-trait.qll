# the bound is the trait the body uses: another trait's evidence for the container is missing
{ trait Size(a) = sig { size : a -> I64 }; trait Show(a) = sig { show : a -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Show(I64) = module { show = fn(n) { 2 } }; impl Size(Option(a)) = module { size = fn(o) { Show.show(o) } }; 0 }
