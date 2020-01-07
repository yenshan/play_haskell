

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)


depth :: Tree a -> Int -> Int
depth Empty d = 0
depth (Branch _ Empty Empty) d = d
depth (Branch _ l r) d = maximum [depth l (d+1), depth r (d+1)]


doLayout :: Tree a -> Int -> Int -> Int -> Tree (a,(Int,Int))
doLayout Empty _ _ _ = Empty
doLayout (Branch x l r) w px py = (Branch (x,(px,py))
                                          (doLayout l (w `div` 2) (px-w) (py+1))
                                          (doLayout r (w `div` 2) (px+w) (py+1)))

internals :: Tree (a,(Int,Int)) -> [Int]
internals Empty = []
internals (Branch (_,(x,_)) l r) = internals l ++ [x] ++ internals r


addX Empty _ = Empty
addX (Branch (x,(px,py)) l r) p = (Branch (x,(px+p,py))
                                          (addX l p)
                                          (addX r p))

layout :: Tree a -> Tree (a,(Int,Int))
layout t = addX t1 (-minX+1)
           where
             w = round $ 2 ** (fromIntegral $ depth t 0) / 2
             t1 = doLayout t w 0 1
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



