# type-case refinement reaches the resume entry of the enclosing handler
({ effect Ask = sig { ask : I64 -> I64 };
   f : [T : Type] -> T -> T = fn[T](x) {
     match (perform Ask.ask(1)) { n => x,
       effect Ask.ask k => match (T) { I64 => resume(1) + 1, _ => resume(0) } } };
   f(7) })
