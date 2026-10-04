# a bound-carrying generic impl inside an imported module survives the import and the open
{ L = import "lib"; open L; open M; Size.size(Some(5)) }
