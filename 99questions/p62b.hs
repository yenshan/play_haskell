
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

atLevel Empty _ = []    
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = do
         print $ atLevel tree4 2
         return ()

         
