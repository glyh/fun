# export (import "mac").{same} re-exports exactly that macro, as open does
{ W = import "wrapper"; W.same(21) }
