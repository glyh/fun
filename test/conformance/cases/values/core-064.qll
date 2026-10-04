# handler escape skips continuation
{
       effect Exit = sig { now : I64 -> I64 };
       program : Unit ->{Exit} I64 = fn(_) { {
         _ = perform Exit.now(99);
         0
       } };
       match (program()) { x => x,
       effect Exit.now value => value
       }
     }
