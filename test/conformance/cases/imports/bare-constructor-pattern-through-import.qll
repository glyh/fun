# a bare pattern head resolves through a binder or an open, never by the scrutinee's type
# - here through an import, which is the shape that first slipped past this rule
{ M = import "user"; match (M.v) { Green => 2, Red => 1 } }
