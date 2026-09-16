# state handler sequences operations
{
       effect State(S) = sig { get : Unit -> S; put : S -> Unit };
       program : Unit ->{State(I64)} I64 = fn(_) { {
         x = perform State.get();
         _ = perform State.put(x + 1);
         perform State.get()
       } };
       h = match (program(())) { x => fn(s : I64) { x },
         effect State.get () => fn(s : I64) { resume(s)(s) },
         effect State.put next => fn(s : I64) { resume(())(next) }
       };
       h(1)
     }
