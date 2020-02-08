
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


display_lisp :: Tree Char -> String
display_lisp (Node a []) = [a]
display_lisp (Node a xs) = ['(',a] ++ concat [" " ++ display_lisp x | x <- xs] ++ [')']


main = do
        print $ display_lisp tree1
        print $ display_lisp tree2
        print $ display_lisp tree3
        print $ display_lisp tree4
        print $ display_lisp tree5
