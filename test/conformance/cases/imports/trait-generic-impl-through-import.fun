# an imported unit's generic pub impl keeps its own variables across the open
{ L = import "lib"; open L; Size.size(Some(5)) }
