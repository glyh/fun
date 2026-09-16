# recursive handler ping pong effects
{
       effect Ping = sig { hit : I64 -> I64 };
       effect Pong = sig { hit : I64 -> I64 };
       rec loop : I64 ->{Ping, Pong} I64 = fn(n) {
          if (n == 0) {
            0
          } else {
            { x = perform Ping.hit(n); y = perform Pong.hit(n - 1); x + y + loop(n - 2) }
          }
        };
        match (loop(4)) { x => x,
       effect Ping.hit n => resume(n),
       effect Pong.hit n => resume(n)
       }
     }
