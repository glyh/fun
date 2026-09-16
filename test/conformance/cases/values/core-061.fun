# a variable pattern binds a closure scrutinee
{ h = match (fn(u : Unit) { 1 }) { x => x }; h(()) }
