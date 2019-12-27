
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty


childs :: Tree a -> Int
childs (Branch _ Empty Empty) = 0
childs (Branch _ Empty _) = 1
childs (Branch _ _ Empty) = 1
childs (Branch _ _ _) = 2


addNode :: a -> Tree a -> [Tree a]
addNode x Empty = [Branch x Empty Empty]
addNode x (Branch a Empty Empty) = [Branch a (leaf x) Empty, Branch a Empty (leaf x)]
addNode x (Branch a left Empty) = [Branch a left (leaf x)]
addNode x (Branch a Empty right) = [Branch a (leaf x) right]
addNode x (Branch a left right)
        | childs left > childs right = [Branch a left r | r <- addNode x right]
        | childs left < childs right = [Branch a l right | l <- addNode x left]
        | otherwise = [Branch a l right | l <- addNode x left] ++ [Branch a left r | r <- addNode x right]

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror _ Empty = False
mirror Empty _ = False
mirror (Branch a la ra) (Branch b lb rb) = mirror la lb && mirror ra rb

symmetric :: Eq a => Tree a -> Bool
symmetric (Branch _ r l) = mirror r l

-----------------------------------
-----------------------------------
construct_ [x] = addNode x Empty
construct_ [x,y,z] = addNode x $ head $ construct_ [y,z]
construct_ (x:xs) = concat [addNode x y | y <- construct_ xs]

construct = head . construct_ . reverse

main = do
         print $ construct [3,2,5,7,1]
         print $ symmetric $ construct [5,3,18,1,4,12,21]
         print $ symmetric $ construct [3,2,5,7,1]
         return ()
