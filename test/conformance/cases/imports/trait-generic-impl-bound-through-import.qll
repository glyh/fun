# an imported unit's bounded generic impl reaches the importer, with its dictionary resolved there
{ L = import "lib"; open L; Size.size(Some(5)) }
