data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty


mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror _ Empty = False
mirror Empty _ = False
mirror (Branch a la ra) (Branch b lb rb) = (a == b) && mirror la lb && mirror ra rb

symmetric :: Eq a => Tree a -> Bool
symmetric (Branch _ r l) = mirror r l

main = do
        print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
        print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
        return ()

