# the bound is the trait the body uses: another trait's evidence for the container is missing
{ trait Size(A) = sig { size : A -> I64 }; trait Show(A) = sig { show : A -> I64 }; impl Size(I64) = module { size = fn(n) { 1 } }; impl Show(I64) = module { show = fn(n) { 2 } }; impl Size(Option(A)) = module { size = fn(o) { Show.show(o) } }; 0 }
