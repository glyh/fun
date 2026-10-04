# imported latent effect function
{ E = import "effects"; StateI64 = E.State(I64); match (E.read(())) { x => x, effect StateI64.get () => 7 } }
