
data Tree a = Node a [Tree a] deriving (Eq,Show)

tree5 = Node 'a' [
                    Node 'f' [Node 'g' []],
                    Node 'c' [],
                    Node 'b' [Node 'd' [], Node 'e' []]
                 ]


bottom_up (Node x []) = [x]
bottom_up (Node x xs) = concat [bottom_up y | y <-xs] ++ [x] 


main = do print $ bottom_up tree5
