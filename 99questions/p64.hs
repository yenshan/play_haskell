
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

inorder :: Tree a -> [a]
inorder Empty = []    
inorder (Branch x Empty Empty) = [x]
inorder (Branch x l r) = inorder l ++ [x] ++ inorder r


findOrder :: Eq a => a -> [(a,Int)] -> Int
findOrder x [] = 0
findOrder x (y:ys) = let (c,i) = y
                     in if x==c then i else findOrder x ys

doLayout :: Eq a => Tree a -> Int -> [(a,Int)] -> Tree (a,(Int,Int))
doLayout Empty _ _ = Empty
doLayout (Branch x l r) d o = (Branch (x,(idx,d))
                                        (doLayout l (d+1) o)
                                        (doLayout r (d+1) o))
                              where
                                idx = findOrder x o 


layout :: Eq a => Tree a -> Tree (a,(Int,Int))
layout t = doLayout t 1 ol
           where
              ol = zip (inorder t) [1..]

tree64 = Branch 'n'
            (Branch 'k'
                (Branch 'c'
                    (Branch 'a' Empty Empty)
                    (Branch 'h'
                        (Branch 'g'
                            (Branch 'e' Empty Empty)
                             Empty
                        )
                        Empty
                    )
                )
                (Branch 'm' Empty Empty)
            )
            (Branch 'u'
                (Branch 'p'
                     Empty
                    (Branch 's'
                        (Branch 'q' Empty Empty)
                        Empty
                    )
                )
                Empty
            )


main = do
         print $ layout tree64
         return ()



