# handler ping pong effects
{
       effect Ping = sig { hit : I64 -> I64 };
       effect Pong = sig { hit : I64 -> I64 };
       program : Unit ->{Ping, Pong} I64 = fn(_) { {
         x = perform Ping.hit(1);
         perform Pong.hit(x + 10)
       } };
       match (program()) { x => x,
       effect Ping.hit n => { y = perform Pong.hit(n + 1); resume(y) }
       effect Pong.hit n => resume(n + 100)
       }
     }
