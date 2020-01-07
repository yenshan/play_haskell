
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty


cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree 2 = [Branch 'x' (leaf 'x') Empty, Branch 'x' Empty (leaf 'x')]    
cbalTree n 
    | odd n = [Branch 'x' l r | l <- cbt1, r <- cbt1]
    | otherwise = [Branch 'x' l r | l <- cbt2, r <- cbt1]
                  ++
                  [Branch 'x' l r | l <- cbt1, r <- cbt2]
    where
        cbt1 = cbalTree (n `div` 2)
        cbt2 = cbalTree ((n `div` 2) - 1)

main = do
         print $ cbalTree 4
         return ()
