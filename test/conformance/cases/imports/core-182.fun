# imported public effect handler
{ E = import "effects"; match (perform E.Exc.raise(1)) { x => x, effect E.Exc.raise n => n + 1 } }
