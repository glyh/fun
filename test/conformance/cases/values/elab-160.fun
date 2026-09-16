{ rec Tree = enum { Leaf, Node(Tree, Tree) }; match (Tree.Node(Tree.Leaf, Tree.Leaf)) { Tree.Node(_, _) => 1, Tree.Leaf => 0 } }
