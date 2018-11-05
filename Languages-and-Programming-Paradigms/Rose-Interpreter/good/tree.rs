// Przykład prostego drzewa

data Tree() = Node(Tree() Tree()) | Leaf(Int());

tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4));

{Tree() -> Int()} sumTree t =
	case t of
	  |> Node(l r) => (sumTree l) + (sumTree r) <|
	  |> Leaf(x) => x <|;
	  
main = sumTree tree