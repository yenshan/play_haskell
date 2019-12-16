data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty


hasChild :: Tree a -> Bool
hasChild (Branch a Empty Empty) = False
hasChild _ = True


addNode :: a -> Tree a -> [Tree a]
addNode x Empty = [Branch x Empty Empty]
addNode x (Branch a Empty Empty) = [Branch a (leaf x) Empty, Branch a Empty (leaf x)]
addNode x (Branch a left Empty) = [Branch a left (leaf x)]
addNode x (Branch a Empty right) = [Branch a (leaf x) right]
addNode x (Branch a left right)
        | hasChild left && not (hasChild right) = [Branch a left r | r <- addNode x right]
        | not (hasChild left) && hasChild right = [Branch a l right | l <- addNode x left]
        | otherwise = [Branch a l right | l <- addNode x left] ++ [Branch a left r | r <- addNode x right]

cbalTree 1 = addNode 'x' Empty
cbalTree 3 = addNode 'x' $ head $ cbalTree 2
cbalTree n = concat [addNode 'x' y | y <- cbalTree (n-1)]

main = do
         print $ cbalTree 4
         return ()
