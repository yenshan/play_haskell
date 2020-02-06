
data Tree a = Node a [Tree a] deriving (Eq,Show)

tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [
                    Node 'f' [Node 'g' []],
                    Node 'c' [],
                    Node 'b' [Node 'd' [], Node 'e' []]
                 ]


nnodes (Node _ []) = 1
nnodes (Node _ xs) = 1 + sum [nnodes x | x <- xs]

main = do
        print $ nnodes tree2
        print $ nnodes tree3
        print $ nnodes tree4
        print $ nnodes tree5
