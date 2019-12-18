import Data.List

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

eqTree :: Eq a => Tree a -> Tree a -> Bool
eqTree Empty Empty = True
eqTree _ Empty = False
eqTree Empty _ = False
eqTree (Branch a la ra) (Branch b lb rb) = a == b && eqTree la lb && eqTree ra rb

cbalTree 1 = addNode 'x' Empty
cbalTree n = nubBy eqTree $ concat [addNode 'x' y | y <- cbalTree (n-1)]

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror _ Empty = False
mirror Empty _ = False
mirror (Branch a la ra) (Branch b lb rb) = (a == b) && mirror la lb && mirror ra rb

symmetric :: Eq a => Tree a -> Bool
symmetric (Branch _ r l) = mirror r l

-------------------------------------------------
  p58
-------------------------------------------------

symCbalTree n = filter symmetric $ cbalTree n

main = do
         print $ symCbalTree 5
         return ()
