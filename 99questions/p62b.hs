
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

internals Empty = []
internals (Branch a Empty Empty) = []
internals (Branch a l r) = [a] ++ internals l ++ internals r

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = do
         print $ internals tree4 
         return ()

         
