# a name the selective open did not list stays unbound
{ M = module { pub helper = fn(x) { x }; pub other = 1 }; open M.{helper}; other }
