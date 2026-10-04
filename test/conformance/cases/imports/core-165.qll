# an open at a unit's top level scopes over that unit's later bindings (`pub v = Red`);
# the consumer names the constructors through the unit that declares them, because a
# bare head resolves through a binder or an open, never by the scrutinee's type
{ M = import "user"; C = import "color"; match (M.v) { C.Red => 1, C.Green => 2 } }
