import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

    
hBalTree x 1 = [Empty, leaf x]
hBalTree x n = [Branch x a b | a <- hBalTree x (n-1), b <- hBalTree x (n-1), a /= Empty || b /= Empty]

main = do
         print $ length $ hBalTree 'x' 3
         print $ take 4 $ hBalTree 'x' 3
         return ()

         
