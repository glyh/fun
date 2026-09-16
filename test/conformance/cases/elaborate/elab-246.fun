{ effect Ping = sig { hit : I64 -> I64 }; (match (perform Ping.hit(1)) { x => x, effect Ping.hit n => perform Ping.hit(n + 1) } : I64) }
