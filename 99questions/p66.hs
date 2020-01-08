
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)


hasRChild :: Eq a => Tree a -> Bool
hasRChild Empty = False
hasRChild (Branch _ _ r) = r /= Empty

hasLChild :: Eq a => Tree a -> Bool
hasLChild Empty = False
hasLChild (Branch _ l _) = l /= Empty


doLayout :: Eq a => Tree a -> Int -> Int -> Tree (a,(Int,Int))
doLayout Empty _ _  = Empty
doLayout (Branch x l r) px py
        | hasRChild l && hasLChild r = (Branch (x,(px,py))
                                               (doLayout l (px-2) (py+1))
                                               (doLayout r (px+2) (py+1)))
        | otherwise = (Branch (x,(px,py))
                              (doLayout l (px-1) (py+1))
                              (doLayout r (px+1) (py+1)))

internals :: Tree (a,(Int,Int)) -> [Int]
internals Empty = []
internals (Branch (_,(x,_)) l r) = internals l ++ [x] ++ internals r


addX Empty _ = Empty
addX (Branch (x,(px,py)) l r) p = (Branch (x,(px+p,py))
                                          (addX l p)
                                          (addX r p))
                            
layout t = addX t1 (-minX+1)
           where
             t1 = doLayout t 0 1
             minX = minimum $ internals t1

tree65 = Branch 'n'
                (Branch 'k'
                    (Branch 'c'
                        (Branch 'a' Empty Empty)
                        (Branch 'e'
                            (Branch 'd' Empty Empty)
                            (Branch 'g' Empty Empty)
                        )
                    )
                    (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                    (Branch 'p'
                        Empty
                        (Branch 'q' Empty Empty)
                    )
                    Empty
                )


main = do
        print $ layout tree65
        return ()



