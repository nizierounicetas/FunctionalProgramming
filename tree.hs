
-- реализовать тип Двоичное дерево 
data BinaryNode a
    = Node a (BinaryNode a) (BinaryNode a)
    | Leaf a
    | Null
    deriving(Show)

getLeft::(BinaryNode a) -> (BinaryNode a)
getLeft (Node _ l _) = l


getRight::(BinaryNode a) -> (BinaryNode a)
getRight (Node _ _ r) = r

-- поэлементное преобразование, сохраняющее структуру (аналог map)
treeMap::(a -> b)->(BinaryNode a)->(BinaryNode b) 
treeMap f (Leaf x) = Leaf (f x)
treeMap f Null = Null
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

-- подсчет элементов
countNodes::(BinaryNode a) -> Int
countNodes (Leaf _) = 1
countNodes (Null) = 0
countNodes (Node _ l r) = (countNodes l) + (countNodes r) + 1


-- обход в глубину
treeTraverseD::(a -> b -> b) -> b -> (BinaryNode a) -> b
treeTraverseD f res (Leaf x) = f x res
treeTraverseD _ res Null = res
treeTraverseD f res (Node x l r) = (treeTraverseD f (f x (treeTraverseD f res l)) r)

-- обход в ширину
treeTraverseWHepler::(a -> b -> b) -> b -> [BinaryNode a] -> b
treeTraverseWHepler _ res [] = res
treeTraverseWHepler f res (Leaf x:t) = treeTraverseWHepler f curRes t where curRes = f x res
treeTraverseWHepler f res (Null:t) = treeTraverseWHepler f res t
treeTraverseWHepler f res ((Node x l r):t) = treeTraverseWHepler f curRes (t ++ [l, r]) where curRes = f x res

treeTraverseW::(a -> b -> b) -> b -> (BinaryNode a) -> b
treeTraverseW f res root = treeTraverseWHepler f res [root]

l1 = Leaf 3
l2 = Leaf 4

l3 = Node 0 l1 l2
l4 = Node 1 Null Null

r1 = Node 1 Null (Node 2 (Leaf 5) (Leaf 3))

r2 = treeMap (\x -> x + 3) r1

t = Node "Eins" (Node "Zwei" (Leaf "Vier") (Leaf "Fuenf")) (Node "Drei" (Leaf "Sechs") (Leaf "Sieben"))

foo x res = res ++ [x]