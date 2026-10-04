# match nested tuple
match ((1, True), 2) { ((x, _), y) => x + y }
