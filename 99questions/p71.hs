
data Tree a = Node a [Tree a] deriving (Eq,Show)

depth n (Node _ []) = (Node n [])
depth n (Node _ xs) = (Node n [depth (n+1) x | x <- xs])


collect (Node x []) = [x]
collect (Node x xs) = [x] ++ concat [collect y | y <- xs]

ipl = sum . collect . depth 0

---- 

tree5 = Node 'a' [
                    Node 'f' [Node 'g' []],
                    Node 'c' [],
                    Node 'b' [Node 'd' [], Node 'e' []]
                 ]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]


main = do
        print $ ipl tree5
        print $ ipl tree4
