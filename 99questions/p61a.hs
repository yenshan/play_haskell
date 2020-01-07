data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

countLeaves Empty = []
countLeaves (Branch a Empty Empty) = [a]
countLeaves (Branch _ l r) = countLeaves l ++ countLeaves r

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = do
         print $ countLeaves tree4 
         return ()

         
