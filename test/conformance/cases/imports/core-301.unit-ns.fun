open (import "std");
              syntax namespace : Decl { namespace $(n : Id) $(b : Block) => { pub $n = module $b } };
              namespace Geometry { pub pi = 3; pub tau = 6 };
              pub answer = Geometry.tau
