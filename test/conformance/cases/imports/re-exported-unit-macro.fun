# a unit's macro reaches a program through a unit that re-exports it
{ W = import "wrapper"; W.same(21) }
