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

cbalTree 1 = addNode 'x' Empty
cbalTree 3 = addNode 'x' $ head $ cbalTree 2
cbalTree n = concat [addNode 'x' y | y <- cbalTree (n-1)]

main = do
         print $ cbalTree 4
         return ()
