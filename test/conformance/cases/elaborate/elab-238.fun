{ effect Console = sig { log : Tuple(2, I64, I64) -> I64 }; match (perform Console.log((1, 2))) { x => x, effect Console.log(only) => only } }
