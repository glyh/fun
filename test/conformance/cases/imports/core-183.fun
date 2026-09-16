# open imported module exposes effect
{ E = import "effects"; open E; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n + 1 } }
